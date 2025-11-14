import os
import json

import pandas as pd
from dotenv import load_dotenv
from tqdm import tqdm
from openai import OpenAI


# --- Configuración ---
CATEGORIES = [
    "overall_rating",
    "cleanliness_rating",
    "communication_rating",
    "location_rating",
    "value_rating",
    "safety_rating",
]

SYSTEM_INSTRUCTIONS = """
Eres un analista experto en reseñas de Airbnb.
Tu tarea es leer la reseña de un huésped y asignar calificaciones de 1 a 5
(en decimales si es necesario) a las siguientes categorías:

- overall_rating: impresión general de la estancia.
- cleanliness_rating: limpieza del alojamiento.
- communication_rating: claridad y rapidez de la comunicación con el anfitrión.
- location_rating: conveniencia de la ubicación (accesibilidad, cercanía a puntos de interés, etc.).
- value_rating: relación calidad-precio percibida por el huésped.
- safety_rating: sensación de seguridad dentro y fuera del alojamiento.

Reglas IMPORTANTES:
- Usa la escala de 1 (muy malo) a 5 (excelente).
- Si la reseña no menciona explícitamente una categoría, infiere de manera razonable
  pero nunca dejes campos fuera.
- Devuelve EXCLUSIVAMENTE un JSON válido, sin texto adicional ni comentarios.
- El JSON debe tener exactamente estas llaves:
  overall_rating, cleanliness_rating, communication_rating,
  location_rating, value_rating, safety_rating.
"""


def get_client() -> OpenAI:
    """
    Inicializa el cliente de OpenAI usando la API key y base_url del .env.
    """
    load_dotenv()
    api_key = os.environ.get("OPENAI_API_KEY")
    base_url = os.environ.get("OPENAI_BASE_URL", "https://api.openai.com/v1")

    if not api_key:
        raise RuntimeError("OPENAI_API_KEY no está definido en el entorno ni en el .env")

    return OpenAI(
        api_key=api_key,
        base_url=base_url,
    )


def analyze_review(client: OpenAI, review_text: str) -> dict:
    """
    Llama al modelo para obtener las calificaciones de una reseña.

    Devuelve un diccionario con las CATEGORIES como llaves y floats como valores.
    """
    review_text = (review_text or "").strip()
    if not review_text:
        # Si no hay texto, devolvemos None en todo
        return {cat: None for cat in CATEGORIES}

    # Leemos el nombre del modelo desde el entorno (para poder cambiarlo fácil)
    model_name = os.environ.get("OPENAI_MODEL_NAME", "azure-2/gpt-4o-mini")

    completion = client.chat.completions.create(
        model=model_name,
        response_format={"type": "json_object"},
        messages=[
            {"role": "system", "content": SYSTEM_INSTRUCTIONS},
            {"role": "user", "content": review_text},
        ],
    )

    content = completion.choices[0].message.content

    try:
        data = json.loads(content)
    except json.JSONDecodeError:
        # Si algo sale mal, devolvemos None en todo para no romper el pipeline
        return {cat: None for cat in CATEGORIES}

    scores = {}
    for cat in CATEGORIES:
        val = data.get(cat)
        try:
            scores[cat] = float(val) if val is not None else None
        except (TypeError, ValueError):
            scores[cat] = None

    return scores


def main():
    # Rutas (ajusta si tu estructura es distinta)
    input_path = "data/raw/reviews1.csv"
    by_review_output_path = "data/processed/review_sentiment_by_review.csv"
    by_listing_output_path = "data/processed/review_sentiment_by_listing.csv"

    os.makedirs("data/processed", exist_ok=True)

    print(f"Leyendo {input_path} ...")
    df = pd.read_csv(input_path)

    # Nos quedamos solo con las columnas que nos interesan
    cols_needed = ["listing_id", "id", "date", "reviewer_id", "reviewer_name", "comments"]
    df = df[cols_needed]

    # Limpiar texto: quitar <br/> y similares
    df["comments_clean"] = (
        df["comments"]
        .fillna("")
        .str.replace("<br/>", " ", regex=False)
        .str.replace("<br>", " ", regex=False)
        .str.strip()
    )

    # ⚠️ MUY IMPORTANTE: tu archivo tiene ~1.4M reseñas.
    # Para probar, usa un subconjunto pequeño:
    df = df.sample(n=300, random_state=0).reset_index(drop=True)

    client = get_client()

    results = []
    print("Analizando reseñas con el modelo (esto puede tardar)...")
    for _, row in tqdm(df.iterrows(), total=len(df)):
        scores = analyze_review(client, row["comments_clean"])
        scores["listing_id"] = row["listing_id"]
        scores["review_id"] = row["id"]
        scores["date"] = row["date"]
        results.append(scores)

    scored_df = pd.DataFrame(results)

    # Guardar nivel reseña
    scored_df.to_csv(by_review_output_path, index=False)
    print(f"Guardado: {by_review_output_path}")

    # Promedio por listing_id
    listing_scores = (
        scored_df.groupby("listing_id")[CATEGORIES]
        .mean(numeric_only=True)
        .reset_index()
    )

    listing_scores.to_csv(by_listing_output_path, index=False)
    print(f"Guardado: {by_listing_output_path}")


if __name__ == "__main__":
    main()

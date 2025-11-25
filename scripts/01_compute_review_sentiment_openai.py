import os
import json
from textwrap import shorten

import math
import argparse

import pandas as pd
from dotenv import load_dotenv
from tqdm import tqdm
from openai import OpenAI

# --- CONFIGURACIÓN ---
CATEGORIES = [
    "overall_rating",
    "cleanliness_rating",
    "communication_rating",
    "location_rating",
    "value_rating",
    "safety_rating",
]

MODEL_NAME = "azure-2/gpt-4o-mini"
MAX_REVIEWS_PER_LISTING = 4
MAX_CHARS_PER_REVIEW = 800

SYSTEM_INSTRUCTIONS = """Eres un experto en analizar reseñas de Airbnb y evaluar propiedades. 
Tu tarea es leer múltiples reseñas de huéspedes sobre un anuncio específico y asignar 
calificaciones numéricas de 1 a 5 (se permiten decimales como 4.5) para las siguientes categorías:

- overall_rating: Calificación general del anuncio
- cleanliness_rating: Limpieza de la propiedad
- communication_rating: Calidad de comunicación con el anfitrión
- location_rating: Ubicación de la propiedad
- value_rating: Relación calidad-precio
- safety_rating: Seguridad del lugar

IMPORTANTE: Devuelve ÚNICAMENTE un objeto JSON válido con estas 6 categorías.
Ejemplo: {"overall_rating": 4.5, "cleanliness_rating": 5.0, "communication_rating": 4.0, 
"location_rating": 4.5, "value_rating": 4.0, "safety_rating": 5.0}"""


def get_client() -> OpenAI:
    load_dotenv()
    api_key = os.environ.get("OPENAI_API_KEY")
    base_url = os.environ.get(
        "OPENAI_BASE_URL",
        "https://llm-gateway.truefoundry.com",
    )

    if not api_key:
        raise RuntimeError("OPENAI_API_KEY no está definido en el entorno ni en el .env")

    client = OpenAI(api_key=api_key, base_url=base_url)
    print(f"Cliente inicializado. base_url={base_url}, modelo={MODEL_NAME}")
    return client


def build_listing_prompt(listing_id: int, reviews_df: pd.DataFrame) -> str:
    lines = [f"LISTING_ID: {listing_id}", "", "RESEÑAS DE HUÉSPEDES:"]
    for _, row in reviews_df.iterrows():
        date = row.get("date", "")
        reviewer = row.get("reviewer_name", "")
        text = str(row.get("comments_clean", "") or "")
        text_short = shorten(text, width=MAX_CHARS_PER_REVIEW, placeholder="...")
        block = f"[RESEÑA - {date} - {reviewer}]\n{text_short}"
        lines.append(block)
        lines.append("")
    lines.append(
        "TAREA: Con base en TODAS las reseñas anteriores, evalúa el ANUNCIO en general "
        "y asigna calificaciones de 1 a 5 (se permiten decimales) a las categorías "
        "overall_rating, cleanliness_rating, communication_rating, location_rating, "
        "value_rating y safety_rating. Devuelve SOLO un JSON válido."
    )
    return "\n".join(lines)


def analyze_listing(client: OpenAI, listing_id: int, reviews_df: pd.DataFrame) -> dict:
    if reviews_df.empty:
        return {cat: None for cat in CATEGORIES}

    user_content = build_listing_prompt(listing_id, reviews_df)

    try:
        completion = client.chat.completions.create(
            model=MODEL_NAME,
            response_format={"type": "json_object"},
            messages=[
                {"role": "system", "content": SYSTEM_INSTRUCTIONS},
                {"role": "user", "content": user_content},
            ],
        )
        content = completion.choices[0].message.content
    except Exception as e:
        print(f"Error en API para listing {listing_id}: {e}")
        return {cat: None for cat in CATEGORIES}

    try:
        data = json.loads(content)
    except json.JSONDecodeError:
        print(f"Error decodificando JSON para listing {listing_id}")
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
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--chunk-idx",
        type=int,
        default=0,
        help="Qué parte quieres correr (0, 1, 2 si num-chunks=3)",
    )
    parser.add_argument(
        "--num-chunks",
        type=int,
        default=1,
        help="En cuántas partes dividir los listings",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Solo calcula número de listings y qué chunk corres, sin llamar a la API",
    )
    args = parser.parse_args()

    input_path = "data/raw/reviews1.csv"
    output_path = "data/processed/review_sentiment_by_review.csv"
    os.makedirs("data/processed", exist_ok=True)

    print(f"Leyendo {input_path} ...")
    df = pd.read_csv(input_path)

    # limpiar texto
    df["comments_clean"] = (
        df["comments"]
        .fillna("")
        .str.replace("<br/>", " ", regex=False)
        .str.replace("<br>", " ", regex=False)
        .str.strip()
    )

    # fechas
    df["date"] = pd.to_datetime(df["date"], errors="coerce")
    df = df.sort_values(["listing_id", "date"], ascending=[True, False])

    # info global
    listing_ids = (
        df["listing_id"]
        .drop_duplicates()
        .sort_values()
        .tolist()
    )
    n_listings = len(listing_ids)
    print(f"Número de listings únicos: {n_listings}")
    print(f"Número de llamadas al modelo (1 por listing): {n_listings}")

    # dividir en chunks
    chunk_size = math.ceil(n_listings / args.num_chunks)
    start = args.chunk_idx * chunk_size  # CORREGIDO: era chunk-idx
    end = min(start + chunk_size, n_listings)
    selected_ids = set(listing_ids[start:end])

    print(
        f"Corriendo chunk {args.chunk_idx+1}/{args.num_chunks}: "
        f"{len(selected_ids)} listings (índices {start} a {end-1})"
    )

    if args.dry_run:
        print("Dry run: no se llama a la API, solo se muestran estos números.")
        return

    # Cargar resultados existentes si el archivo existe
    existing_listings = set()
    if os.path.exists(output_path):
        existing_df = pd.read_csv(output_path)
        existing_listings = set(existing_df["listing_id"].unique())
        print(f"Archivo existente encontrado con {len(existing_listings)} listings ya procesados")

    # Filtrar solo listings que no han sido procesados
    selected_ids = selected_ids - existing_listings
    print(f"Listings pendientes en este chunk: {len(selected_ids)}")

    if not selected_ids:
        print("Todos los listings de este chunk ya fueron procesados. Nada que hacer.")
        return

    # filtrar al chunk correspondiente
    df_chunk = df[df["listing_id"].isin(selected_ids)].copy()
    grouped = df_chunk.groupby("listing_id", group_keys=False)

    # cliente de OpenAI
    client = get_client()

    results = []

    for listing_id, group in tqdm(grouped, total=len(selected_ids)):
        sample_reviews = group.head(MAX_REVIEWS_PER_LISTING).copy()
        scores = analyze_listing(client, listing_id, sample_reviews)
        scores["listing_id"] = listing_id
        scores["n_reviews_used"] = len(sample_reviews)
        results.append(scores)

    if not results:
        print("No hay resultados nuevos para guardar.")
        return

    listing_scores = pd.DataFrame(results)

    # Append a archivo existente o crear nuevo
    if os.path.exists(output_path):
        listing_scores.to_csv(output_path, mode='a', header=False, index=False)
        print(f"Agregados {len(results)} listings a {output_path}")
    else:
        listing_scores.to_csv(output_path, index=False)
        print(f"Creado nuevo archivo: {output_path}")

    print(f"Total de listings procesados hasta ahora: {len(existing_listings) + len(results)}")


if __name__ == "__main__":
    main()

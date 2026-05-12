import os
import pandas as pd
import pyalex
from pyalex import Works
from dotenv import load_dotenv

load_dotenv()

# Set your email for OpenAlex's polite pool (faster rate limits, no key needed)
# Add OPENALEX_EMAIL=you@example.com to a .env file or your environment
pyalex.config.email = os.getenv("OPENALEX_EMAIL", "")

CHUNK_SIZE = 50  # OpenAlex filter URL length limit


def fetch_refs_by_openalex_ids(openalex_ids):
    """Batch-fetch referenced works in chunks to stay under URL length limits."""
    short_ids = [id_.split("/")[-1] for id_ in openalex_ids]
    all_works = []
    for i in range(0, len(short_ids), CHUNK_SIZE):
        chunk = short_ids[i:i + CHUNK_SIZE]
        batch = Works().filter(openalex="|".join(chunk)).get()
        all_works.extend(batch)
    return all_works


def resolve_work(doi, openalex_id, title):
    """Try DOI → OpenAlex ID → title search, in that order."""
    if pd.notna(doi) and str(doi).strip():
        results = Works().filter(doi=str(doi).strip()).get()
        if results:
            return results[0]
        print(f"  DOI not found in OpenAlex, trying next fallback...")

    if pd.notna(openalex_id) and str(openalex_id).strip():
        short_id = str(openalex_id).strip().split("/")[-1]
        try:
            return Works()[short_id]
        except Exception:
            print(f"  OpenAlex ID lookup failed, trying title search...")

    results = Works().search_filter(title=title).get()
    if results:
        matched_title = results[0].get("title", "")
        print(f"  Matched by title search — verify this is correct: '{matched_title}'")
        return results[0]

    return None


df = pd.read_csv("doi.csv")
edges = []

for _, row in df.iterrows():
    doi          = row.get("doi", "")
    openalex_id  = row.get("openalex_id", "")
    title        = row["title"]
    reading_list = row.get("reading_list", "")

    print(f"Processing: {title}")

    work = resolve_work(doi, openalex_id, title)
    if not work:
        print(f"  Could not find in OpenAlex: {title}")
        continue

    ref_ids = work.get("referenced_works", [])
    if not ref_ids:
        print(f"  No references found for: {title}")
        continue

    print(f"  Fetching {len(ref_ids)} references...")
    refs = fetch_refs_by_openalex_ids(ref_ids)

    for ref in refs:
        edges.append({
            "source_doi":   work.get("doi", doi),
            "source_title": title,
            "reading_list": reading_list,
            "target_doi":   ref.get("doi", ""),
            "target_title": ref.get("title", ""),
        })

    print(f"  Added {len(refs)} edges for: {title}")

edge_df = pd.DataFrame(edges)
edge_df.to_csv("citation_network.csv", index=False)
print(f"\nEdgelist saved to citation_network.csv ({len(edges)} total edges)")

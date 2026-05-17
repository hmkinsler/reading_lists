import os
from datetime import date

VALID_LISTS = {
    'ai_infrastructure': 'ai_infrastructure',
    'new_materialism': 'new_materialism',
    'critical_making': 'critical_making',
}

VALID_SOURCE_TYPES = ['journal_article', 'book', 'book_chapter']


def prompt_loop(prompt, valid_options):
    while True:
        value = input(prompt).strip()
        if value in valid_options:
            return value
        print(f"  Invalid choice '{value}'. Enter one of: {', '.join(valid_options)}")


def collect_fields(source_type):
    fields = {}
    fields['author']  = input("Author(s) in APA format (e.g. 'Smith, J. D., & Jones, A.'): ").strip()
    fields['year']    = input("Year of publication: ").strip()
    fields['doi_url'] = input("DOI or URL: ").strip()

    if source_type == 'journal_article':
        fields['article_title'] = input("Article title: ").strip()
        fields['journal']       = input("Journal name: ").strip()
        fields['volume']        = input("Volume (leave blank if none): ").strip()
        fields['issue']         = input("Issue (leave blank if none): ").strip()
        fields['pages']         = input("Page range (e.g. 1-15, leave blank if none): ").strip()

    elif source_type == 'book':
        fields['book_title'] = input("Book title: ").strip()
        fields['publisher']  = input("Publisher: ").strip()

    elif source_type == 'book_chapter':
        fields['chapter_title'] = input("Chapter title: ").strip()
        fields['book_title']    = input("Book title: ").strip()
        fields['pages']         = input("Page range (e.g. 45-72): ").strip()
        fields['publisher']     = input("Publisher: ").strip()
        is_edited = input("Is this an edited collection? (y/n): ").strip().lower() == 'y'
        fields['is_edited'] = is_edited
        if is_edited:
            fields['editor'] = input("Editor(s) in APA format (e.g. 'Smith, J. D., & Jones, A.'): ").strip()
        else:
            fields['editor'] = ''

    return fields


def build_apa_citation(source_type, fields):
    doi_url = fields['doi_url']

    if source_type == 'journal_article':
        citation = f"{fields['author']} ({fields['year']}). {fields['article_title']}. *{fields['journal']}*"
        if fields['volume']:
            citation += f", *{fields['volume']}*"
            if fields['issue']:
                citation += f"({fields['issue']})"
        if fields['pages']:
            citation += f", {fields['pages']}"
        citation += "."

    elif source_type == 'book':
        citation = f"{fields['author']} ({fields['year']}). *{fields['book_title']}*. {fields['publisher']}."

    elif source_type == 'book_chapter':
        if fields['is_edited']:
            ed_label = "Eds." if ("&" in fields['editor'] or fields['editor'].count(",") > 1) else "Ed."
            citation = (
                f"{fields['author']} ({fields['year']}). {fields['chapter_title']}. "
                f"In {fields['editor']} ({ed_label}), *{fields['book_title']}*"
            )
        else:
            citation = (
                f"{fields['author']} ({fields['year']}). {fields['chapter_title']}. "
                f"In *{fields['book_title']}*"
            )
        if fields['pages']:
            citation += f" (pp. {fields['pages']})"
        citation += f". {fields['publisher']}."

    if doi_url:
        citation += f" {doi_url}"

    return citation


def get_display_title(source_type, fields):
    if source_type == 'journal_article':
        return fields['article_title']
    elif source_type == 'book':
        return fields['book_title']
    elif source_type == 'book_chapter':
        return fields['chapter_title']


# --- Main ---

file_name = input("File name (format: 'author_year'): ").strip()

reading_list = prompt_loop(
    "Reading list ('ai_infrastructure', 'new_materialism', or 'critical_making'): ",
    VALID_LISTS,
)

source_type = prompt_loop(
    "Source type ('journal_article', 'book', or 'book_chapter'): ",
    VALID_SOURCE_TYPES,
)

fields        = collect_fields(source_type)
citation      = build_apa_citation(source_type, fields)
display_title = get_display_title(source_type, fields)
date_created  = date.today().isoformat()

base_dir    = os.path.dirname(os.path.abspath(__file__))
output_dir  = os.path.join(base_dir, VALID_LISTS[reading_list])
output_path = os.path.join(output_dir, f"{file_name}_notes.md")

if os.path.exists(output_path):
    confirm = input(f"  '{output_path}' already exists. Overwrite? (y/n): ").strip().lower()
    if confirm != 'y':
        print("Aborted. No file was written.")
        raise SystemExit

content = f"""---
title: {display_title}
tags: [{reading_list}]
date_created: {date_created}
---
# Notes for {display_title} ({fields['author']}, {fields['year']})

## Citation Information
{citation}

# Reading Notes
## SQR3
### Survey: Skim the text and identify main ideas

### Query: Write questions to consider as you read

### Read: Take notes on how you answer your questions as you read

### Recite: Consider what you want to remember, and put that into your own words

### Relate: Identify connections or links to other readings and scholarship

## Intervention, Argument, Evidence
### Intervention: What is the intervention that the scholar is making, and where is that intervention being made? In other words, how do you place that intervention relative to existing disciplinary conversations that are taking place?

### Argument: What are the core argument(s) being made in the text?

### Evidence: What evidence is used to support the argument(s)?

### How: What methods and theories are used to make the intervention or argument and support it?

## Key Terms / Concepts

## Qualifying Exam Relevance

# References
"""

with open(output_path, "w", encoding="utf-8") as f:
    f.write(content)

print(f"Notes file created: {output_path}")

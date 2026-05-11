# Template file for reading notes
file_name = input("What is the name for your file? Use the following format: 'Author Year'")

content = """

"""

# Create file with template content and using file name input from user
with open(f"{file_name}_notes.md", "w", encoding="utf-8") as f:
    f.write(content)

# Confirm note file creation
print(f"File '{file_name}_notes.md' created successfully!")

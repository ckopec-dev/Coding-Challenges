#!/bin/bash

echo "Processing list."

# Change to working directory
cd /mnt/SATA08/intranet/Jobs/Coding-Challenges

# To create numbered_lines.txt
#seq 1 100 > numbered_lines.txt

# Specify the input file
INPUT_FILE="Euler/Python/problems.txt"
OUTPUT_PATH="Euler/Python"

# Get first line from input
LINE=$(head -n 1 $INPUT_FILE)
echo "SCRIPT: Processing line: $LINE"

# Create the prompt
PROMPT="Solve Eulergit p Problem $LINE using python."
echo "SCRIPT: Prompt: $PROMPT"

# Generate ollama result
OUTPUT_PATH="$OUTPUT_PATH/$LINE.md"
echo "SCRIPT: Using ollama to generate output."
ollama run qwen3-coder $PROMPT > $OUTPUT_PATH

# Authenticate to github
echo "SCRIPT: Authenticating to Github."
gh auth login --hostname github.com --with-token < ../github_token.txt

# Get fresh pull
echo "SCRIPT: Pulling latest version."
git pull

# Create a new branch
BRANCH_NAME="Euler-Python-$LINE"
echo "SCRIPT: Creating a new branch named $BRANCH_NAME."
git checkout -b $BRANCH_NAME

# Add updated items
echo "SCRIPT: Adding files to staging."
git add .

# Make commit
echo "SCRIPT: Making commit."
git commit -m "Added $OUTPUT_PATH"

# Push the commit
echo "SCRIPT: Pushing commit."
git push --set-upstream origin $BRANCH_NAME

# Creating the pull request
echo "SCRIPT: Creating pull request."
gh pr create --title "$BRANCH_NAME ready for review" --body "Generating automatically from create.sh."

# Switch back to main branch
echo "SCRIPT: Switching back to main."
git switch main

# Remove the first line
echo "SCRIPT: Pruning list."
sed -i '1d' $INPUT_FILE

echo "SCRIPT: Processing complete."

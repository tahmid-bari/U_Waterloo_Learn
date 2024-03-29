#!/usr/bin/env python
"""
File: indexEngine.py
Author: Tahmid Bari
Student ID: 20864394
Date Last Modified: June 11, 2021
Python Version: 3.7
Indexes documents from the LA Times dataset by DOCNO and custom ID. For University of Waterloo course MSCI 720
"""

import argparse
import gzip
import os.path
import pickle
import re
import time

from document import Document
from collections import Counter

DOC_OPEN_TAG = "<DOC>"
DOC_CLOSE_TAG = "</DOC>"
DOC_NO_TAG = "<DOCNO>"
HEADLINE_TAG = "<HEADLINE>"

DOC_ID_NO_PATH = '/doc_id_no.p'
TOKEN_ID_TOKEN_PATH = '/token_id_token.p'
TOKEN_TOKEN_ID_PATH = '/token_token_id.p'
TOKEN_ID_POSTINGS_PATH = '/token_id_postings.p'


def parse_args():
    parser = argparse.ArgumentParser(description='Retrieve documents and their associated metadata for the LA Times '
                                                 'gzip archive')
    parser.add_argument('gzip', metavar='GZIP_FILE', help='Path of LA Times .gzip file')
    parser.add_argument('directory', metavar='DOCUMENT_DIRECTORY', help='Directory to save documents and metadata')
    args = parser.parse_args()
    return args.gzip, args.directory


def create_raw_text_doc(doc_id, document, directory_path):
    date_obj = time.strptime(document.date, '%B %d, %Y')
    formatted_date = time.strftime('/%y/%m/%d/', date_obj)

    file_path = directory_path + formatted_date
    os.makedirs(file_path, exist_ok=True)
    file_path += document.docno.split('-')[1]
    file_path += '.p'

    with open(file_path, "wb") as text_file:
        pickle.dump(document, text_file)
    print("Processed Document: {}".format(doc_id))


def get_match_strip_tags(re_search, text, re_strip_tags, re_strip_close_tags):
    matches = re.search(re_search, text)
    if matches:
        match = matches.group().strip()
        match = re_strip_tags.sub('', match)
        match = re_strip_close_tags.sub('', match)
        return match
    return ''


def build_doc_metadata(document):
    text = document.raw_document
    re_strip_tags = re.compile('<\w+>\n*')
    re_strip_close_tags = re.compile('<\/\w+>\n*')

    # Get HEADLINE information from raw document
    headline_match = re.compile('<HEADLINE>[\s\S]+<\/HEADLINE>')
    document.headline = get_match_strip_tags(headline_match, text, re_strip_tags, re_strip_close_tags)

    # Get GRAPHIC information from raw document
    headline_match = re.compile('<GRAPHIC>[\s\S]+<\/GRAPHIC>')
    document.graphic = get_match_strip_tags(headline_match, text, re_strip_tags, re_strip_close_tags)

    # Get TEXT information from raw document
    headline_match = re.compile('<TEXT>[\s\S]+<\/TEXT>')
    document.text = get_match_strip_tags(headline_match, text, re_strip_tags, re_strip_close_tags)


def build_inversion_index(doc_id, document):
    tokens = tokenize(document)
    document.length = len(tokens)

    token_ids = convert_tokens_to_ids(tokens)
    add_to_postings(doc_id, token_ids)


def tokenize(document):
    tokens = []
    for text in document.headline, document.graphic, document.text:
        # Lowercase and split on non-alphanumerics
        text = text.lower()
        text_tokens = re.split('[\W]', text)
        tokens += text_tokens

    # Remove empty strings in resulting tokens list
    tokens = list(filter(None, tokens))
    return tokens


def convert_tokens_to_ids(tokens):
    token_ids = []
    for token in tokens:
        if token in token_token_id:
            token_ids.append(token_token_id[token])
        else:
            token_id = len(token_token_id)
            token_token_id[token] = token_id
            token_id_token[token_id] = token
            token_ids.append(token_id)
    return token_ids


def add_to_postings(doc_id, token_ids):
    token_counts = Counter(token_ids)

    for token_id, token_count in token_counts.items():
        if token_id not in token_id_postings:
            token_id_postings[token_id] = []
        token_id_postings[token_id].append(doc_id)
        token_id_postings[token_id].append(token_count)


gzip_path, directory_path = parse_args()

# Create the directory if it doesn't exist, else throw an error
os.makedirs(directory_path, exist_ok=False)

# Check if gzip exists
if not os.path.exists(gzip_path):
    print("GZIP file doesn't exist in the specified location! Please correct this issue.")
    exit(1)

# Create global dictionaries
doc_id_no = {}
token_id_token = {}
token_token_id = {}
token_id_postings = {}  # Token ID => Postings List (List of docIDs followed by token count in those docs)

with gzip.open(gzip_path, mode='rt') as gzip_file:
    document = None
    raw_document = []
    doc_id = 0

    for line in gzip_file:
        raw_document.append(line)

        if DOC_OPEN_TAG in line:
            # Create a Document object
            document = Document()
        elif DOC_NO_TAG in line:
            # Generate document internal id
            docno = re.search('(LA|RF)\d{6}-\d{4}', line).group()
            docno_list = docno.split('-')
            date = docno_list[0][2:]
            doc_id += 1
            document.doc_id = doc_id
            document.docno = docno

            # Generate formatted date
            date_obj = time.strptime(date, '%m%d%y')
            formatted_date = time.strftime('%B %d, %Y', date_obj)
            document.date = formatted_date

        elif DOC_CLOSE_TAG in line:
            raw_document_string = "".join(raw_document)
            document.raw_document = raw_document_string

            # Insert into docno to id map
            doc_id_no[doc_id] = document.docno

            # Build document metadata
            build_doc_metadata(document)

            # Build in-memory inversion index
            build_inversion_index(doc_id, document)

            # Insert into directory as YY/MM/DD/NNNN.p
            create_raw_text_doc(doc_id, document, directory_path)

            # clear the raw document list
            raw_document.clear()

# Create all pickles
doc_id_no_path = directory_path + DOC_ID_NO_PATH
token_id_token_path = directory_path + TOKEN_ID_TOKEN_PATH
token_token_id_path = directory_path + TOKEN_TOKEN_ID_PATH
token_id_postings_path = directory_path + TOKEN_ID_POSTINGS_PATH

pickle.dump(doc_id_no, open(doc_id_no_path, 'wb'))
pickle.dump(token_id_token, open(token_id_token_path, 'wb'))
pickle.dump(token_token_id, open(token_token_id_path, 'wb'))
pickle.dump(token_id_postings, open(token_id_postings_path, 'wb'))
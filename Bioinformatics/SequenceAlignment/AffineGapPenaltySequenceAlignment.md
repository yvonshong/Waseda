# Affine Gap Penalty Sequence Alignment
Authored by **SONG DAIWEI** 44161588-3

# Answer the following questions about scoring an alignment.

## Affine Gap Penalty
*Calculate the score for the DNA sequence alignment shown below, using the scoring matrix below.  Use an affine gap penalty to score the gaps, with -11 for opening the gap and -1 for each additional position in the gap.  (“Affine gap penalty” refers to a situation when the gap opening and gap extension penalties are not the same.  The gap opening penalty should be greater than the gap extension penalty.)*

## Nonaffine Gap Penalty
*How would the score change if you were to use a nonaffine gap penalty?  To answer the question, try a nonaffine penalty of -2, and then -6.*

```
GACTACGATCCGTATACGCACA---GGTTCAGAC        
||||||    ||||||||||||   |||||||||       
GACTACAGCTCGTATACGCACACATGGTTCAGAC          
```
| |A|G|T|C|
|-|-|-|-|-|
|C|-7|-7|-5|2|
|T|-7|-7|2| |
|G|-5|2|||
|A|2||||



# Answer the following questions using the BLOSUM62 matrix.

## BLOSUM62 matrix
*Using this matrix, two aligned cysteines (C) would receive a score of 9 while two aligned threonines (T) would only receive a score of 5.  What can you conclude about cysteine relative to threonine?*


## Another BLAST Scoring Matrix
*A serine (S) aligned with a cysteine (C) would receive a negative score (-1) while a serine aligned with a threonine would receive a positive score (1).  Offer a possible explanation for this in terms of physicochemical properties of the amino acid side chains.*

Part of BLOSUM62 matrix:

| |C|S|T|
|-|-|-|-|
|T|-1|1|5|
|S|-1|4| |
|C|9| | |




## Dynamic Programming Algorithm
*The bestng an alignment score.  Use the BLOSUM62 matrix alignment of the two amino acid sequences “LDS” and “LNS” is obvious (it’s shown below).  Given a scoring system, you could easily calculate an alignment score.  Set up a matrix and use the dynamic programming algorithm to “prove” that this is the best alignment by calculating x (see Powerpoint notes) to score aligned residues, and use a gap penalty of -1.  (You may hand write the matrix in your homework rather than typing it if you like.)*
```
seq1   LDS
       | |
seq2   LNS
```




# Example
*Find the optimal global alignment of the two sequences Seq1: THISLINE and Seq2: ISALIGNED based on the BLOSUM62 matrix with linear gap penalty of -4.*
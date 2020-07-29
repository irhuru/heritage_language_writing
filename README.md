# Heritage Language Writing

## The study

This project **compares the written assignments produced by a group of Spanish heritage speakers enrolled in a heritage-only language course with those produced by a group of Spanish heritage speakers enrolled in a mixed language course** (i.e., heritage and non-heritage speakers). Both courses lasted one semester.

## The data

There were **four written assignments per student**:

* ***Comp1Ver1***: the first version (i.e., before feedback) of the first composition submitted in the semester.
* ***Comp1Ver2***: the second version (i.e., after feedback) of the first composition submitted in the semester.
* ***Comp2Ver1***: the first version of the last composition submitted in the semester.
* ***Comp2Ver2***: the second version of the last composition submitted in the semester.

## The analysis

The goal of the study was to compare **whether and to what extent students improved their writing skills over the course of the semester**. The first written assignments were compared with the last written assignments. Also, the first versions were compared with the second versions to determine the effectiveness of feedback.

Assignments were compared attending to four main categories (based on [Bello, 2019](https://www.ideals.illinois.edu/handle/2142/105814)):

* **Lexical density**: The percentage of lexical words in relation to the total number of words in a text. Since the lexical words are the words conveying information, the assumption is that a text will become denser (provide more information), as the number of lexical words increases in relation to the total number of words.
* **Lexical sophistication**: The proportion of infrequent words in a text. Word frequency was determined in reference to a particular corpus, [Corpus del Español](https://www.corpusdelespanol.org) (Davies, 2006), which was compiled from a set of 20,000,000 words of spoken and written Spanish. Generally, the words that fall within the first 2,000 most frequent words are considered frequent or basic, while words above the 2,000 band are considered advanced or sophisticated. Therefore, a text will be considered more sophisticated as it increases in the number of sophisticated words.
* **Lexical diversity**: It refers to the number of different words in a text and it is measured as the ratio of types (different unique words) to the total number of tokens (words) in a text. In general, the more types there are in a text in relation to the total number of tokens, the more diverse the vocabulary is.
* **Syntactic complexity**: The range and the sophistication of grammatical resources exhibited in language production. Complexity must be focused on the use of a variety of grammatical and lexical features, the main assumption being that as a writer’s proficiency increases, they will produce more grammatically and lexically complex sentences. Clauses per T-unit is the most commonly used measure. A T-unit was manually coded as every independent clause plus all the subordinated clauses attached to it, and all the coordinated clauses were counted as separate units.

## Technical information

All analyses were conducted using [R](https://www.r-project.org) running in [RStudio](https://rstudio.com).

Packages used include:

* [*tidyverse*](https://www.tidyverse.org)
* [*tidytext*](https://cran.r-project.org/web/packages/tidytext/index.html)
* [*stringr*](https://cran.r-project.org/web/packages/stringr/index.html)
* [*tokenizers*](https://cran.r-project.org/web/packages/tokenizers/index.html)
* [*lme4*](https://cran.r-project.org/web/packages/lme4/index.html)
* [*udpipe*](https://cran.r-project.org/web/packages/udpipe/index.html), based on the [Universal Dependencies](http://universaldependencies.org/#ud-treebanks) framework

The code is documented in an R file.

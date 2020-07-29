# Loading packages

library(tidyverse)
library(tidytext)
library(stringr)
library(tokenizers)
library(udpipe)
library(lme4)

# UDPipe (Spanish)

udpipe_download_model(language = "spanish-gsd")
gsd_model = udpipe_load_model(file = 'spanish-gsd-ud-2.4-190531.udpipe')

# Vector containing text titles

list_texts = list.files("heritage_texts") 

# Organizing data by assignment

comp1ver1 = list()
comp1ver2 = list()
comp2ver1 = list()
comp2ver2 = list()

for (i in list_texts){
  if(str_detect(i, "Comp1Ver1")){
    comp1ver1 = c(comp1ver1, i)
  } else if(str_detect(i, "Comp1Ver2")){
    comp1ver2 = c(comp1ver2, i)
  } else if(str_detect(i, "Comp2Ver1")){
    comp2ver1 = c(comp2ver1, i)
  } else if(str_detect(i, "Comp2Ver2")){
    comp2ver2 = c(comp2ver2, i)
  }
}

# Creating an empty data frame

texts_df = data.frame(text = character(), student = character(), group = character(), 
                      assignment = character(), version = character(), time = character())

# Inserting texts in the data frame (indicating the source file and the assignment title)

writing_assignments = list(comp1ver1, comp1ver2, comp2ver1, comp2ver2)

for (WA in writing_assignments){
  for (i in 1:length(WA)){
    # Text
    if (str_detect(WA[i], "10")){
      content_lines = file(file.path("heritage_texts", WA[i]), encoding = "macintosh")
      on.exit(close(content_lines))
      content = readLines(content_lines)
      content_final = paste0(content, collapse = " ")
      
    } else if (str_detect(WA[i], "20")){
      content_lines = readLines(file.path("heritage_texts", WA[i]))
      content_final = paste0(content_lines, collapse = " ")
    }
    
    # Student
    student_string = as.character(WA[i])
    student_code = substr(student_string, 1, 3)  
    
    # Group
    if (student_code < 200){
      student_group = "G1"
    } else{
      student_group = "G2"
    }
    
    # Assignment
    assignment_text = as.character(WA[i])
    my_regex = gregexpr("(?<=_).*", assignment_text, perl = TRUE)
    regex_match = as.character(regmatches(assignment_text, my_regex))
    assignment_final = gsub("\\.txt", "", regex_match)
    
    # Version
    version_code = str_sub(student_string, -5, -5)
    
    # Time
    if (assignment_final == "Comp1Ver1" | assignment_final == "Comp1Ver2"){
      time_code = "first"
    } else{
      time_code = "last"
    }
    
    # Creating a temporal frame
    temporal_df = data.frame(text = content_final, student = student_code, group = student_group, 
                             assignment = assignment_final, version = version_code, time = time_code)
    
    # Joining the two data frames
    texts_df = bind_rows(texts_df, temporal_df)
  }
}


# BASIC DESCRIPTIVE STATISTICS

# Number of sentences per text

n_sentences = list()

for (i in texts_df$text){
  sentences = tokenize_sentences(i)
  num_sentences = length(sentences[[1]])
  n_sentences = c(n_sentences, num_sentences)
}

texts_df$n_sentences = n_sentences

# Plotting number of sentences per text

texts_df$assignment = as.factor(texts_df$assignment)
texts_df$student = as.factor(texts_df$student)
texts_df$n_sentences = as.numeric(texts_df$n_sentences)

bar_graph_sentences = ggplot(texts_df, aes(x = assignment, y = n_sentences, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Number of sentences per text")

# Number of tokens per text (total and average)

n_tokens_total = list()
n_tokens_avg_sentence = list()

for (i in texts_df$text){
  tokens_total = tokenize_words(i)
  num_tokens = length(tokens_total[[1]])
  n_tokens_total = c(n_tokens_total, num_tokens) # total number of tokens per text
  
  sentences = tokenize_sentences(i)
  tokens_per_sentence = list()
  for (sentence in unlist(sentences)){
    tokens_sentence = tokenize_words(sentence)
    num_tokens_sentence = length(unlist(tokens_sentence))
    tokens_per_sentence = c(tokens_per_sentence, num_tokens_sentence)
  }
  mean_tokens = mean(unlist(tokens_per_sentence))
  round_mean_tokens = round(mean_tokens, 2)
  n_tokens_avg_sentence = c(n_tokens_avg_sentence, round_mean_tokens) # average number of tokens per sentence
}

texts_df$n_tokens_total = n_tokens_total
texts_df$n_tokens_avg_sentence = n_tokens_avg_sentence

# Plotting number of tokens

bar_graph_tokens_total = ggplot(texts_df, aes(x = assignment, y = n_tokens_total, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Total number of tokens per text")

bar_graph_tokens_avg = ggplot(texts_df, aes(x = assignment, y = n_tokens_avg_sentence, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Average number of tokens per sentence")

# LEXICAL DENSITY

# Creating a data frame for udpipe

empty_frame = data.frame(doc.id = character(), paragraph_id = integer(), sentence_id = integer(), sentence = character(),
                         token_id = integer(), token = character(), lemma = character(), upos = character(), xpos = character(),
                         feats = character(), head_token_id = integer(), dep_rel = character(), deps = character(), misc = character(),
                         student = character(), assignment = character())

# Loop that POS-tags each token in a text

iteration = 1

for (text in texts_df$text){
  student = texts_df$student[iteration]
  assignment = texts_df$assignment[iteration]
  analysis = udpipe_annotate(gsd_model, text)
  analysis = as.data.frame(analysis)
  analysis$student = student
  analysis$assignment = assignment
  empty_frame = rbind(empty_frame, analysis)
  iteration = iteration + 1
}

# Removing non-lexical tokens from the data frame

lexical_df = empty_frame %>%
  filter(upos == "ADJ" | upos == "ADV" | upos == "INTJ" | upos == "NOUN" | upos == "PROPN" | upos == "VERB")

# Calculating number of lexical tokens by assignment and student

counting_lexical_tokens = lexical_df %>%
  group_by(student, assignment) %>%
  summarise(lexical_tokens = length(upos))

# Joining the two data frames

texts_df = texts_df %>%
  inner_join(counting_lexical_tokens, by=c("student", "assignment"))

# Calculating lexical density

texts_df$lexical_tokens = as.numeric(texts_df$lexical_tokens)
texts_df$n_tokens_total = as.numeric(texts_df$n_tokens_total)

texts_df$lexical_density = round(((texts_df$lexical_tokens / texts_df$n_tokens_total) * 100), 2)

# Plotting lexical density

  # Graph by participant

bar_graph_lexical_density = ggplot(texts_df, aes(x = assignment, y = lexical_density, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical density")

  # Graph by group and time

texts_df$lexical_density = as.numeric(texts_df$lexical_density)

lexical_density_stats = texts_df %>%
  group_by(time, group) %>%
  summarise(mean_ld = mean(lexical_density))

bar_graph_lexical_density2 = ggplot(lexical_density_stats, aes(x = time, y = mean_ld, fill = time)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical density")

  # Graph by group and version

lexical_density_stats2 = texts_df %>%
  group_by(version, group) %>%
  summarise(mean_ld = mean(lexical_density))

bar_graph_lexical_density3 = ggplot(lexical_density_stats2, aes(x = version, y = mean_ld, fill = version)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = "Version", y = "Lexical density")

# Inferential statistics

texts_df$student = as.factor(texts_df$student)
texts_df$version = as.factor(texts_df$version)
texts_df$assignment = as.factor(texts_df$assignment)
texts_df$time = as.factor(texts_df$time)

  # Time differences

m_ld1 = lmer(data = texts_df, lexical_density ~ time * group + (1|student) + (1|assignment))
summary(m_ld1) 

  # Version differences

m_ld2 = lmer(data = texts_df, lexical_density ~ version * group + (1|student) + (1|assignment))
summary(m_ld2) 

# LEXICAL SOPHISTICATION

# Importing dataframe with the 2,000 most common words in Spanish

spanish_2000 = readxl::read_xlsx("freq_words_span.xlsx")
spanish_2000 = spanish_2000 %>% 
  pull(word)

# Creating a new dataframe to store the results

lemma_frame = data.frame(student = character(), assignment = character(), total_lemmas = numeric(), 
                         sophisticated_lemmas = numeric(), lexical_sophistication = numeric())

# Loop that extracts total and unique lemmas from each student's assignment and calculates lexical sophistication

iteration = 1

for (text in texts_df$text){
  
  student = texts_df$student[iteration]
  assignment = texts_df$assignment[iteration]
  
  annotation = udpipe_annotate(gsd_model, text)
  annotation = as.data.frame(annotation)
  
  annotation = annotation %>% filter(upos != "PUNCT")
  
  total_unique_lemmas = unique(annotation$lemma)
  sophisticated_lemmas = 0
  comparing_lemmas = for (i in total_unique_lemmas){if (!(i %in% spanish_2000)){sophisticated_lemmas = sophisticated_lemmas + 1}}
  lexical_sophistication = (sophisticated_lemmas / length(total_unique_lemmas)) * 100
  
  my_lemma_frame = data.frame(student = student, assignment = assignment, total_unique_lemmas = length(total_unique_lemmas),
                              sophisticated_lemmas = sophisticated_lemmas, lexical_sophistication = lexical_sophistication)
  
  lemma_frame = rbind(lemma_frame, my_lemma_frame)
  
  iteration = iteration + 1
}

# Joining this information to the main data frame

texts_df = texts_df %>%
  inner_join(lemma_frame, by=c("student","assignment"))

# Plotting lexical sophistication

  # Graph by participant

bar_graph_lexical_sophistication = ggplot(texts_df, aes(x = assignment, y = lexical_sophistication, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical sophistication")

  # Graph by group and time

lexical_sophistication_stats = texts_df %>%
  group_by(time, group) %>%
  summarise(mean_ls = mean(lexical_sophistication))

bar_graph_lexical_sophistication2 = ggplot(lexical_sophistication_stats, aes(x = time, y = mean_ls, fill = time)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical sophistication")

  # Graph by group and version

lexical_sophistication_stats2 = texts_df %>%
  group_by(version, group) %>%
  summarise(mean_ls = mean(lexical_sophistication))

bar_graph_lexical_sophistication3 = ggplot(lexical_sophistication_stats2, aes(x = version, y = mean_ls, fill = version)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = "Version", y = "Lexical sophistication")

# Inferential statistics

  # Time differences

m_ls1 = lmer(data = texts_df, lexical_sophistication ~ time * group + (1|student) + (1|assignment))
summary(m_ls1) 

  # Version differences

m_ls2 = lmer(data = texts_df, lexical_sophistication ~ version * group + (1|student) + (1|assignment))
summary(m_ls2) 


# LEXICAL DIVERSITY


# Creating an empty data frame to store lexical diversity information

lemma_frame2 = data.frame(student = character(), assignment = character(), 
                          lexical_diversity = character())

# Loop that calculates lexical diversity (measured as TTR or 'type-token ratio')

iteration = 1

for (text in texts_df$text){
  
  student = texts_df$student[iteration]
  assignment = texts_df$assignment[iteration]
  
  annotation2 = udpipe_annotate(gsd_model, text)
  annotation2 = as.data.frame(annotation2)
  annotation2 = annotation2 %>% filter(upos != "PUNCT")
  
  lemmatized_text = annotation2$lemma
  tokens_lemmas = length(lemmatized_text)
  
  total_unique_lemmas = texts_df$total_unique_lemmas[iteration]
  
  my_lemma_frame2 = data.frame(student = student, assignment = assignment, 
                               lexical_diversity = (total_unique_lemmas/tokens_lemmas))
  
  lemma_frame2 = rbind(lemma_frame2, my_lemma_frame2)
  
  iteration = iteration + 1
}

# Joining this information to the main dataframe

texts_df = texts_df %>%
  inner_join(lemma_frame2, by=c("student","assignment"))

# Plotting lexical diversity

  # Graph by participant

bar_graph_lexical_diversity = ggplot(texts_df, aes(x = assignment, y = lexical_diversity, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical diversity")

  # Graph by group and time

lexical_diversity_stats = texts_df %>%
  group_by(time, group) %>%
  summarise(mean_ls = mean(lexical_diversity))

bar_graph_lexical_diversity2 = ggplot(lexical_diversity_stats, aes(x = time, y = mean_ls, fill = time)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Lexical diversity")

  # Graph by group and version

lexical_diversity_stats2 = texts_df %>%
  group_by(version, group) %>%
  summarise(mean_ls = mean(lexical_diversity))

bar_graph_lexical_diversity3 = ggplot(lexical_diversity_stats2, aes(x = version, y = mean_ls, fill = version)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = "Version", y = "Lexical diversity")

# Inferential statistics

  # Time differences

m_ld1 = lmer(data = texts_df, lexical_diversity ~ time * group + (1|student) + (1|assignment))
summary(m_ld1) 

  # Version differences

m_ld2 = lmer(data = texts_df, lexical_diversity ~ version * group + (1|student) + (1|assignment))
summary(m_ld2) 

# SYNTACTIC COMPLEXITY

# Loading syntactic complexity data (manually coded)

syn_complex = readxl::read_xlsx("syntactic_complexity.xlsx")
syn_complex$student = as.factor(syn_complex$student)

# Joining data to previous data frame

texts_df = texts_df %>%
  inner_join(syn_complex, by=c("student","assignment"))

# Plotting syntactic complexity

  # Graph by participant

bar_graph_syntactic_complexity = ggplot(texts_df, aes(x = assignment, y = syntactic_complexity, fill = assignment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~student) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Syntactic complexity")

  # Graph by group and time

syntactic_complexity_stats = texts_df %>%
  group_by(time, group) %>%
  summarise(mean_sc = mean(syntactic_complexity))

bar_graph_syntactic_complexity2 = ggplot(syntactic_complexity_stats, aes(x = time, y = mean_sc, fill = time)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = NULL, y = "Syntactic complexity")

  # Graph by group and version

syntactic_complexity_stats2 = texts_df %>%
  group_by(version, group) %>%
  summarise(mean_sc = mean(syntactic_complexity))

bar_graph_syntactic_complexity3 = ggplot(syntactic_complexity_stats2, aes(x = version, y = mean_sc, fill = version)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~group) + 
  theme_minimal() + 
  scale_fill_grey() + 
  theme(legend.position="none") + 
  labs(x = "Version", y = "Syntactic complexity")

# Inferential statistics

  # Time differences

m_sc1 = lmer(data = texts_df, syntactic_complexity ~ time * group + (1|student) + (1|assignment))
summary(m_sc1) 

  # Version differences

m_sc2 = lmer(data = texts_df, syntactic_complexity ~ version * group + (1|student) + (1|assignment))
summary(m_sc2) 

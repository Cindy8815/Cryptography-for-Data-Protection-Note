# 1. CAESAR CIPHER R  Programming =====================================

## It is a type of substitution cipher in which each letter in the plaintext is 'shifted' a certain number of places down the alphabet.

## step 1: match each letter in the plain text message P with the number  1-26

letter_to_number <- function(letter){
  letter <- toupper(letter)
  if(letter %in% LETTERS){
    Number <- as.integer(charToRaw(letter))
    Number <- Number - 65
    # -65 to convert the ASCII code of an uppercase letter into its position in the alphabet (0 for "A", 1 for "B", etc.).
    return(Number)
  }else{
    return(NA)
  }
}

string_to_numbers <- function(string){
  letters_vector <- unlist(strsplit(string,""))
  numbers_vector <- sapply(letters_vector,letter_to_number)
  numbers_vector <- numbers_vector[!is.na(numbers_vector)]
  return(numbers_vector)
}

# Example Usage: hello
string_numbers<-string_to_numbers("HELLO")
string_numbers

## step 2 : choose K, and complete (P+K) mod 26 for each letter

caesar_k<-function(k){
  number_pks<-string_numbers+k
  for (i in 1:length(number_pks)){
    if (number_pks[i] >= 26){
      number_pks[i] <- number_pks[i] %% 26}
    else {number_pks[i] <-number_pks[i] 
    }
  }
  number_pks<-number_pks[!is.na(number_pks)]
  return(number_pks)
}

# Example Usage:k=3
number_string<-caesar_k(3)
number_string<-as.character(number_string)



## step 3: C =(P+K) mod 26 is a new character of ciphertext.
### change the new numbers to letters (ciphertext)

number_to_letter <- function(number){
  if(number>=0 && number<26){
    return(LETTERS[number+1])  
  }else{
    return(NA)
  }
}

numbers_to_string <- function(number_string){
  numbers_vector <- unlist(strsplit(number_string," "))
  numbers_vector <- as.numeric(numbers_vector)
  letters_vector <- sapply(numbers_vector,number_to_letter)
  
  phrase <- paste0(letters_vector[1],letters_vector[2])
  if(length(letters_vector)<3){
    return(phrase)
  }
  for(i in 3:length(letters_vector)){
    phrase <- paste0(phrase[1],letters_vector[i])
  }
  return(phrase)
}
      
numbers_to_string(number_string)


# CAESAR CIPHER FUNCTION (Combined)==========================================

## ====Encryption====

caesar_k <- function(input_string, k) {
  # Convert the input string to uppercase
  input_string <- toupper(input_string)
  
  # Convert the input string to numeric values representing the characters
  string_numbers <- as.integer(charToRaw(input_string)) - 65
  
  # Shift the numeric values by k positions
  shifted_numbers <- (string_numbers + k) %% 26
  
  # Convert back to characters and remove any NA values
  result_string <- intToUtf8(shifted_numbers + 65)
  result_string <- result_string[!is.na(result_string)]
  
  return(result_string)
}

# Example usage:
plain_text <- "okanagan college"
cipher_text <- caesar_k(plain_text, 4)
print(cipher_text)


## ====Decryption====
### Just need take the previous cipher_text as plain_text and k as -k.

plain_text <- "SOEREKERXGSPPIKI"
cipher_text <- caesar_k(plain_text, -4)
print(cipher_text)


# 2. ATBASH CIPHER FUNCTION (Combined)==========================================


atbash_cipher <- function(input_string) {
# Define the alphabet
  alphabet <- LETTERS
    
  # Convert the input string to uppercase
  input_string <- toupper(input_string)
  # split string to letters
  input_letter <- unlist(strsplit(input_string,""))
  # find the index position for each letter
  for (char in input_letter){
    if (char %in% alphabet){
      index <-match(input_letter,alphabet)
      # find the atbash letters 
      atbash_char <- alphabet[26 - (index - 1)]
      # combine letters together
      result_string <- paste(atbash_char,collapse ="")

    }
    else{
      result_string<-paste(result_string,char,collapse = "")
      
    }
    
  }
  
  return(result_string)
}

# Example usage:

## encryption 
input_string<-"attack at dawn"
encryption<-atbash_cipher(input_string)
# get rid of the "NA" in the string(if have)
encryption<- gsub("NA","",encryption)
encryption

##decryption
input_string<-"ZGGZXPZGWZDM"
atbash_cipher(input_string)




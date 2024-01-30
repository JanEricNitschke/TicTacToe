// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "tictactoe.h"

// Function to handle conversion errors
_Noreturn void handleConversionError(const char* option) {
  perror("Error: Invalid number format for ");
  printf("%s\n", option);
  exit(EXIT_FAILURE);
}

// Function to parse an option and update player strength
void parseOption(const char option, const char* arg, int* strength) {
  char* endptr = 0;
  const int BASE_TEN = 10;
  *strength = (int)strtol(arg, &endptr, BASE_TEN);

  if (*endptr != '\0' && !isspace((unsigned char)*endptr)) {
    handleConversionError(&option);
  }
}

// Function to parse options and update player strengths
int parseOptions(const int argc, const char* argv[argc + 1],
                 int* playerX_strength, int* playerO_strength) {
  for (int i = 1; i < argc; ++i) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
        case 'X':
          if (++i < argc) {
            parseOption('X', argv[i], playerX_strength);
          } else {
            printf("Option -X requires an argument.\n");
            return EXIT_FAILURE;
          }
          break;
        case 'O':
          if (++i < argc) {
            parseOption('O', argv[i], playerO_strength);
          } else {
            printf("Option -O requires an argument.\n");
            return EXIT_FAILURE;
          }
          break;
        default:
          printf("Unknown option %s\n", argv[i]);
          return EXIT_FAILURE;
      }
    } else {
      printf("Unexpected argument: %s\n", argv[i]);
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

// Add command line parsing for Human vs AI player later
int main(const int argc, const char* argv[argc + 1]) {
  int playerX_strength = -1;
  int playerO_strength = -1;

  // Parse options
  if (parseOptions(argc, argv, &playerX_strength, &playerO_strength) !=
      EXIT_SUCCESS) {
    return EXIT_FAILURE;
  }

  if (!play_game(playerX_strength, playerO_strength)) {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

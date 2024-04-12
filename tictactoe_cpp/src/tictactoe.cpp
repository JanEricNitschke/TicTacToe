// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#include <tictactoe.hpp>

#include <algorithm>
#include <chrono>  // NOLINT [build/c++11]
#include <iostream>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <vector>

#include <Random.hpp>

std::ostream &operator<<(std::ostream &os, const GameState &obj) {
  os << static_cast<std::underlying_type<GameState>::type>(obj);
  return os;
}

auto getAISettings(char player) -> AISettings {
  AISettings settings{};
  // For cpplint
  std::string player_string{player};
  settings.isAI =
      getPlayerYesNo("Is player " + player_string + " an AI?[y/n]: ");
  if (settings.isAI) {
    settings.strength = getAIStrength();
  }
  return settings;
}

// Function to get yes/no response from the player
auto getPlayerYesNo(std::string_view question) -> bool {
  std::string solo{};
  while (solo != "Y" && solo != "N") {
    std::cout << question << std::endl;
    std::cin >> solo;
    std::ranges::transform(solo, solo.begin(), ::toupper);
  }
  return static_cast<bool>(solo == "Y");
}

// Get information whether it is a
// 1 person or 2 person game
auto getPlayerNumber() -> bool {
  return getPlayerYesNo("Play alone vs AI?[y/n]: ");
}

// Ask user for AI strength
// 1 is Random
// 2 wins if possible
// 3 wins or blocks if possible
// 4 plays perfect
auto getAIStrength() -> int {
  std::string input{};
  int strength{};
  std::cout << "AI strength settings:" << std::endl;
  std::cout << "1: Easy" << std::endl;
  std::cout << "2: Medium" << std::endl;
  std::cout << "3: Hard" << std::endl;
  std::cout << "4: Impossible" << std::endl;
  while (true) {
    std::cout << "How strong should the AI be?[1-4]: " << std::endl;
    std::cin >> input;
    try {
      strength = std::stoi(input);
    } catch (std::invalid_argument const &ex) {
      std::cout << "Invalid input" << std::endl;
      std::cin.clear();  // Clear the error state
      std::cin.ignore(std::numeric_limits<std::streamsize>::max(),
                      '\n');  // Clear input buffer
      continue;
    }
    if (strength > 4 || strength < 1) {
      std::cout << "Invalid input" << std::endl;
      continue;
    }
    break;
  }
  return strength;
}

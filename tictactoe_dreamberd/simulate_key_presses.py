"""Helper scripts to get dreamberd version useable in CI."""

import argparse
from pynput.keyboard import Controller
import time
from typing import Iterable

# Initialize the keyboard controller
keyboard_controller = Controller()


def simulate_key_presses(keys: Iterable[int]) -> None:
    """Simulate key presses for winning scenario.

    Args:
        keys (Iterable[int]): Sequence of keys to press
    """
    time.sleep(5)

    for key in keys:
        keyboard_controller.press(str(key))
        keyboard_controller.release(str(key))
        time.sleep(0.75)


# Main function to handle command line arguments
def main():
    parser = argparse.ArgumentParser(
        description="Simulate key presses based on game state."
    )
    parser.add_argument(
        "--win",
        action="store_true",
        default=False,
        help="Simulate key presses for a win",
    )
    parser.add_argument(
        "--draw",
        action="store_true",
        default=False,
        help="Simulate key presses for a draw",
    )

    args = parser.parse_args()
    keys: Iterable[int] = []
    if args.win:
        keys = range(7)
    elif args.draw:
        keys = (0, 4, 8, 1, 7, 6, 2, 5, 3)
    else:
        print("Please specify either --win or --draw")
    simulate_key_presses(keys)


# Run the main function
if __name__ == "__main__":
    main()

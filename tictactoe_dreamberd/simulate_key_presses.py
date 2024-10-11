"""Helper scripts to get dreamberd version useable in CI."""

from pynput.keyboard import Controller
import time

# Initialize the keyboard controller
keyboard_controller = Controller()


# Function to simulate number key presses from 0 to 8
def simulate_number_key_presses() -> None:
    """Run key presses from 0 to 6 with delays."""
    time.sleep(2)

    for key in range(7):
        keyboard_controller.press(str(key))
        keyboard_controller.release(str(key))
        time.sleep(0.5)


# Simulate the number key presses
simulate_number_key_presses()

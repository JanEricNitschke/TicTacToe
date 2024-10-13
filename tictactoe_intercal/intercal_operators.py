"""Function for debugging intercal logic."""


def intercal_dollar_operator(num1: int, num2: int) -> int:
    """Simulates the $ operator which interleaves the bits of num1 and num2.

    Args:
        num1 (int): Operand 1
        num2 (int): Operand 2

    Returns:
        int: Result of the operation
    """
    print(f"{bin(num1)}${bin(num2)}")
    result = 0
    for i in range(32):
        bit1 = (num1 >> i) & 1
        bit2 = (num2 >> i) & 1
        result |= (bit2 << (2 * i)) | (bit1 << (2 * i + 1))
    print(bin(result))
    return result


def intercal_question_operator(num: int) -> int:
    """Simulates the ? operator which rotates right and XORs.

    Args:
        num (int): Operand

    Returns:
        int: Result of the operation
    """
    # Rotate right by 1
    print(f"?{bin(num)}")
    rotated = (num >> 1) | ((num & 1) << 15)  # 16-bit rotation
    # XOR with the original number
    result = num ^ rotated
    print(bin(result))
    return result


def intercal_ampersand_operator(num: int) -> int:
    """Simulates the & operator which rotates right and XORs.

    Args:
        num (int): Operand

    Returns:
        int: Result of the operation
    """
    # Rotate right by 1
    print(f"&{bin(num)}")
    rotated = (num >> 1) | ((num & 1) << 15)  # 16-bit rotation
    # XOR with the original number
    result = num & rotated
    print(bin(result))
    return result


def intercal_tilde_operator(num1: int, num2: int) -> int:
    """Simulates the ~ operator which selects bits from num1 where num2 has 1 bits.

    Args:
        num1 (int): Operand 1
        num2 (int): Operand 2

    Returns:
        int: Result of the operation
    """
    print(f"{bin(num1)}~{bin(num2)}")
    result = 0
    n_found = 0
    for i in range(16):
        if (num2 >> i) & 1:
            result |= ((num1 >> i) & 1) << n_found
            n_found += 1
    print(bin(result))
    return result


def intercal_operations(user_input: int) -> int:
    """Spot already taken check: "?'user_input$3'"~5.

    Args:
        user_input (int): Operand

    Returns:
        int: Result of the operation
    """
    step1 = intercal_dollar_operator(user_input, 3)
    step2 = intercal_question_operator(step1)
    result = intercal_tilde_operator(step2, 5)
    return result


def intercal_operations2(user_input: int) -> int:
    """ZERO CHECK: "?'user_input$1'"~1.

    Args:
        user_input (int): Operand

    Returns:
        int: Result of the operation
    """
    step1 = intercal_dollar_operator(user_input, 1)
    step2 = intercal_question_operator(step1)
    result = intercal_tilde_operator(step2, 1)
    return result


def intercal_operations3(user_input: int) -> int:
    """NON-ZERO CHECK: 'user_input~user_input'~1.

    Args:
        user_input (int): Operand

    Returns:
        int: Result of the operation
    """
    step1 = intercal_tilde_operator(user_input, user_input)
    result = intercal_tilde_operator(step1, 1)
    return result


def intercal_operations4(value: int, player: int) -> int:
    """Check of equal to player: "'?"',10SUB#1'$.10"'~'#0$#65535'"~#65535.

    Args:
        value (int): Value on the board
        player (int): Current Player

    Returns:
        int: Result of the operation
    """
    step1 = intercal_dollar_operator(value, player)
    step2 = intercal_question_operator(step1)
    step3 = intercal_tilde_operator(
        step2, 1431655765
    )  # 1431655765 = intercal_dollar_operator(0, 65535)
    result = intercal_tilde_operator(step3, 65535)
    return result


def ops5(value: int) -> int:
    """AND the value with 0: "&'.2$.9'"~#1.

    Args:
        value (int): Operand

    Returns:
        int: Result of the operation
    """
    step1 = intercal_dollar_operator(0, value)
    step2 = intercal_ampersand_operator(step1)
    result = intercal_tilde_operator(step2, 1)
    return result


def ops6(value: int) -> int:
    """0 -> 2 AND 1 -> 3: '#1$.1'~#3.

    Args:
        value (int): Operand

    Returns:
        int: Result of the operation
    """
    step1 = intercal_dollar_operator(1, value)
    result = intercal_tilde_operator(step1, 3)
    return result


ops6(1)

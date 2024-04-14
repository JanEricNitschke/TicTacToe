<?php

declare(strict_types=1);

include 'src/tictactoe.php';

$X_strength = 0;
$O_strength = 0;
$index = 0;
while ($index < count($argv)) {
    if ($argv[$index] === "-X") {
        $X_strength = intval($argv[$index + 1]);
        $index += 2;
    } elseif ($argv[$index] === "-O") {
        $O_strength = intval($argv[$index + 1]);
        $index += 2;
    } else {
        $index++;
    }
}
play_game($X_strength === 0 ? null : $X_strength, $O_strength === 0 ? null : $O_strength);

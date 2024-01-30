#!/usr/bin/env python3
"""Setup scripts for C extension."""

from setuptools import setup, Extension

setup(
    name="tictactoe_c",
    version="0.0.1",
    packages=["tictactoe_c"],
    package_dir={"tictactoe_c": "tictactoe_c"},
    ext_modules=[
        Extension("tictactoe_c", ["tictactoe_c/tictactoe_py.c", "src/tictactoe.c"])
    ],
    package_data={"tictactoe_c": ["py.typed", "tictactoe_py.pyi"]},
    include_dirs=["include/"],
)

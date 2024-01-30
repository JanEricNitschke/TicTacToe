#!/usr/bin/env python3
"""Setup scripts for C extension."""

from setuptools import setup, Extension

setup(
    name="tictactoe_c",
    version="0.0.1",
    ext_modules=[
        Extension("tictactoe_c", ["python_ext/tictactoe_py.c", "src/tictactoe.c"])
    ],
    include_dirs=["include/"],
)

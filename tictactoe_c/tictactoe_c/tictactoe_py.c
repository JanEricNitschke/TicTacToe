// Copyright 2022-2024 Jan-Eric Nitschke. All rights reserved.

#define PY_SSIZE_T_CLEAN /* Make "s#" use Py_ssize_t rather than int. */
#include <Python.h>

#include "tictactoe.h"

// Helper function to convert Python object to int or None
int parse_optional_int(PyObject* obj, int* result) {
  if (obj == Py_None) {
    *result = -1;  // or any other default value you prefer
    return 1;      // Success
  }
  if (PyLong_Check(obj)) {
    *result = PyLong_AsLong(obj);
    if (*result < 0) {
      PyErr_Format(PyExc_ValueError, "AI Strength must be >= 0, not %i",
                   *result);
      return 0;  // Failure
    }
    return 1;  // Success
  }
  PyErr_Format(PyExc_ValueError, "AI Strength must be an integer.");
  return 0;  // Failure
}

// Signature is required by python.
// NOLINTBEGIN(bugprone-easily-swappable-parameters,misc-unused-parameters)
PyObject* play_game_py(PyObject* self, PyObject* args, PyObject* kwargs) {
  static char* keyword_args[] = {"X_strength", "O_strength", NULL};

  int X_strength = -1;
  int O_strength = -1;

  // Parse the keyword arguments
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|$O&O&", keyword_args,
                                   parse_optional_int, &X_strength,
                                   parse_optional_int, &O_strength)) {
    return NULL;  // Error handling
  }

  // Call the play_game function with the provided arguments
  play_game(X_strength, O_strength);
  Py_RETURN_NONE;
}
// NOLINTEND(bugprone-easily-swappable-parameters,misc-unused-parameters)

// Not actually 100% sure if these couldnt be const but it is how it is
// in the tutorials.
// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
char tictactoe_c_func_docs[] =
    "Play a game of Tic Tac Toe.\n\n"
    "Parameters:\n"
    "  X_strength (Optional[int]): Strength of Player X. Use `None` for human "
    "player. Default: None\n"
    "  O_strength (Optional[int]): Strength of Player O. Use `None` for human "
    "player. Default: None\n";

PyMethodDef tictactoe_c_funcs[] = {
    {"play_game", (PyCFunction)(void (*)(void))play_game_py,
     METH_VARARGS | METH_KEYWORDS, tictactoe_c_func_docs},
    {NULL, NULL, 0, NULL}};

char tictactoe_cmod_docs[] = "Simple module to play tictactoe.";

PyModuleDef tictactoe_c_mod = {PyModuleDef_HEAD_INIT,
                               "tictactoe_c",
                               tictactoe_cmod_docs,
                               -1,
                               tictactoe_c_funcs,
                               NULL,
                               NULL,
                               NULL,
                               NULL};
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

PyMODINIT_FUNC PyInit_tictactoe_c(void) {
  return PyModule_Create(&tictactoe_c_mod);
}

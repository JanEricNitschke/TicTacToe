/* eslint-env node */
import js from "@eslint/js";
// import eslintConfigPrettier from "eslint-config-prettier";
// import eslintConfigGoogle from "eslint-config-google";
import babelParser from "@babel/eslint-parser";
import globals from "globals";
// import jsdoc from "eslint-plugin-jsdoc";

export default [
  {
    ignores: ["dist/**", "node_modules/**", "bin/**", "build/**"],
  },
  js.configs.recommended,
  // eslintConfigPrettier,
  // eslintConfigGoogle,
  {
    files: ["./tictactoe_javascript/script.js"],
    // plugins: {
    //   jsdoc,
    // },
    languageOptions: {
      globals: {
        ...globals.browser,
      },
      parser: babelParser,
      parserOptions: {
        requireConfigFile: false,
        babelOptions: {
          babelrc: false,
          configFile: false,
        },
      },
    },
  },
];

module.exports = {
  root: true,
  extends: ["eslint:recommended", "google", "prettier"],
  parser: "@babel/eslint-parser",
  parserOptions: {
    requireConfigFile: false,
    babelOptions: {
      babelrc: false,
      configFile: false,
    },
  },
  env: {
    browser: true,
    es6: true,
  },
};

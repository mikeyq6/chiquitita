# Chiquitita Syntax Highlighting Setup

## For VS Code

1. Install the "TextMate Languages" extension or create a custom extension:

### Option 1: Quick Setup (Manual Configuration)

1. Open VS Code
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on Mac)
3. Type "Preferences: Configure Language Specific Settings"
4. Select "Chiquitita" or create a new language association
5. Add this to your `settings.json`:

```json
{
  "files.associations": {
    "*.chiq": "chiquitita"
  }
}
```

### Option 2: Create a VS Code Extension

1. Install the VS Code Extension Generator:
   ```bash
   npm install -g yo generator-code
   ```

2. Generate a new language extension:
   ```bash
   yo code
   ```
   - Select "New Language Support"
   - Language name: Chiquitita
   - Language identifier: chiquitita
   - File extensions: .chiq
   - Scope name: source.chiquitita

3. Replace the generated `.tmLanguage.json` file with the `chiquitita.tmLanguage.json` from this repository

4. Install the extension:
   - Press `F5` in VS Code to launch Extension Development Host
   - Or package and install: `vsce package` then install the `.vsix` file

### Option 3: Direct Installation (Simplest)

1. Copy `chiquitita.tmLanguage.json` to your VS Code extensions folder:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\chiquitita-language\syntaxes\`
   - **macOS**: `~/.vscode/extensions/chiquitita-language/syntaxes/`
   - **Linux**: `~/.vscode/extensions/chiquitita-language/syntaxes/`

2. Create a `package.json` in the `chiquitita-language` folder:

```json
{
  "name": "chiquitita-language",
  "displayName": "Chiquitita Language Support",
  "description": "Syntax highlighting for Chiquitita programming language",
  "version": "1.0.0",
  "engines": {
    "vscode": "^1.50.0"
  },
  "categories": ["Programming Languages"],
  "contributes": {
    "languages": [{
      "id": "chiquitita",
      "aliases": ["Chiquitita", "chiquitita"],
      "extensions": [".chiq"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "chiquitita",
      "scopeName": "source.chiquitita",
      "path": "./syntaxes/chiquitita.tmLanguage.json"
    }]
  }
}
```

3. Create `language-configuration.json` in the same folder:

```json
{
  "comments": {
    "lineComment": "//"
  },
  "brackets": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"]
  ],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"" }
  ],
  "surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""]
  ]
}
```

4. Restart VS Code

## Syntax Highlighting Features

The grammar file provides highlighting for:

- **Keywords**: `if`, `elsif`, `else`, `while`, `foreach`, `in`, `chiquitita`, `print`
- **Types**: `int`, `text`, `float`, `bool`, `void`
- **Constants**: `true`, `false`
- **Numbers**: 
  - Decimal: `42`
  - Hexadecimal: `x1f`
  - Binary: `b1010`
  - Octal: `o77`
  - Floating-point: `3.14`
- **Strings**: Double-quoted strings with interpolation support `{{...}}`
- **Comments**: Line comments starting with `//`
- **Operators**: `+`, `-`, `*`, `/`, `=`, `.`, `>`, `<`, `>=`, `<=`, `and`, `or`, `xor`
- **Functions**: Function names before parentheses

## Testing

Open any `.chiq` file in VS Code and you should see syntax highlighting applied automatically.

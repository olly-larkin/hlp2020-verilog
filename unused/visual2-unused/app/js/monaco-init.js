// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

//
// get tippy.js node module for awesome tooltips
// global.tippy = require('tippy.js');
// NB this does not package properly:
// SOLUTION: use tippy-all-min.js
// add to app/js directory
// add <script src="js/tippy.all.min.js"></script> to index.html
// (optional, but best practice) to allow update via yarn, 
// copy this file from ./node_modules/tippy.js to app/js via webpack.config.js CopyWebpackPlugin
//


// todo: why does this work when tippy.js does not?
var path = require('path');

function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  //baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
  baseUrl: uriFromPath(path.join(__dirname, 'js'))

});
// workaround monaco-css not understanding the environment
self.module = undefined;
// workaround monaco-typescript not understanding the environment
self.process.browser = true;
amdRequire(['vs/editor/editor.main'], function () {

  monaco.languages.register({
    id: 'verilog'
  });
  monaco.languages.setMonarchTokensProvider('verilog', {
    // Some things to check !!!
    // 1. Is _ allowed in non-tick numbers, e.g. 123_456
    // 2. Are escapes allowed in strings. e.g. \x1234
    preprocessorKeywords: [
      '`resetall', '`define', '`timescale', '`ifdef', '`else', '`endif',
      '`include', '`signed', '`unsigned', '`celldefine', '`endcelldefine',
      '`default_nettype', '`unconnected_drive', '`nounconnected_drive',
      '`accelerate', '`noaccelerate', '`protect', '`endprotect',
      '`protected', '`endprotected', '`expand_vectornets', '`noexpand_vectornets',
      '`autoexpand_vectornets', '`remove_gatenames', '`noremove_gatenames',
      '`remove_netnames', '`noremove_netnames'
    ],

    keywords: [
      'always', 'and', 'assign', 'attribute', 'begin', 'buf', 'bufif0', 'bufif1',
      'case', 'casex', 'casez', 'cmos', 'deassign', 'default', 'defparam',
      'disable', 'edge', 'else', 'end', 'endattribute', 'endcase', 'endmodule',
      'endfunction', 'endprimitive', 'endspecify', 'endtable', 'endtask',
      'event', 'for', 'force', 'forever', 'fork', 'function', 'highz0', 'highz1',
      'if', 'initial', 'inout', 'input', 'integer', 'join', 'large', 'macromodule',
      'medium', 'module', 'nand', 'negedge', 'nmos', 'nor', 'not', 'notif0',
      'notif1', 'or', 'output', 'parameter', 'pmos', 'posedge', 'primitive',
      'pull0', 'pull1', 'pullup', 'pulldown', 'rcmos', 'reg', 'release', 'repeat',
      'rnmos', 'rpmos', 'rtran', 'rtranif0', 'rtranif1', 'scalared', 'small',
      'specify', 'specparam', 'strength', 'strong0', 'strong1', 'supply0',
      'supply1', 'table', 'task', 'time', 'tran', 'tranif0', 'tranif1', 'tri',
      'tri0', 'tri1', 'triand', 'trior', 'trireg', 'use', 'vectored', 'wait',
      'wand', 'weak0', 'weak1', 'while', 'wire', 'wor', 'xnor', 'xor'
    ],

    builtinFunctionKeywords: [
      '$bitstoreal', '$countdrivers', '$display', '$dumpall', '$dumpfile',
      '$dumpflush', '$dumpoff', '$dumpon', '$dumpvars', '$fclose', '$fdisplay',
      '$finish', '$fmonitor', '$fopen', '$fstrobe', '$fwrite', '$getpattern',
      '$history', '$hold', '$incsave', '$input', '$itor', '$key', '$list',
      '$log', '$monitor', '$monitoroff', '$monitoron', '$nokey', '$nolog',
      '$period', '$printtimescale', '$readmemb', '$readmemh', '$realtime',
      '$realtobits', '$recovery', '$reset', '$reset_count', '$reset_value',
      '$restart', '$rtoi', '$save', '$scale', '$scope', '$setup', '$setuphold',
      '$showscopes', '$showvariables', '$showvars', '$skew', '$sreadmemb',
      '$sreadmemh', '$stime', '$stop', '$strobe', '$time', '$timeformat',
      '$width', '$write', '$fgetc', '$ungetc', '$fgets', '$fscanf',
      '$fread', '$ftell', '$fseek', '$rewind', 'ferror', 'fflush'
    ],

    operators: [
      '!', '~', '&', '|', '^', '~&', '~|', '~^', '+', '-', '*', '/', '%',
      '<<', '>>', '<', '<=', '>', '>=', '==', '!=', '===', '!==', '&&', '||',
      ':', '?', '@', '->', '^~'
    ],

    // define our own brackets as '<' and '>' do not match in Verilog
    brackets: [
      ['(', ')', 'bracket.parenthesis'],
      ['{', '}', 'bracket.curly'],
      ['[', ']', 'bracket.square']
    ],

    // we include these common regular expressions
    symbols: /[=><!~?:&|+\-*/^%@]+/,

    // C style strings
    escapes: /\\(?:[abfnrtv\\"'?]|[0-7]{1,3}|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer for Verilog
    tokenizer: {
      root: [
        // keywords and identifiers
        [/[A-Za-z_$][A-Za-z0-9_$]*/, {
          cases: {
            '@preprocessorKeywords': 'type',
            '@keywords': 'keyword',
            '@builtinFunctionKeywords': 'predefined',
            '@default': 'identifier'
          }
        }],

        // whitespace
        { include: '@whitespace' },

        // delimiters and operators
        [/[{}()[\]]/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],

        // numbers
        [/[0-9]*'[sS]?[bodhBODH][0-9a-fA-F_]+/, 'number'],
        [/[0-9][0-9_]*/, 'number'],
        [/\d*\.\d+([eE][-+]?\d+)?/, 'number.float'],

        // delimiter: after number because of .\d floats
        [/[;,.]/, 'delimiter'],

        // strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'], // non-teminated string
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

        // attributes
        [/\(\*/, { token: 'attribute.open', bracket: '@open', next: '@attribute' }]
      ],

      comment: [
        [/[^/*]+/, 'comment'],
        [/\/\*/, 'comment', '@push'], // nested comment
        ['\\*/', 'comment', '@pop'],
        [/[/*]/, 'comment']
      ],

      attribute: [
        [/.+/, 'attribute'],
        [/\*\)/, { token: 'attribute.close', bracket: '@close', next: '@pop' }]
      ],

      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        [/\/\*/, 'comment', '@comment'],
        [/\/\/.*$/, 'comment']
      ]
    }
  });

  // Convert CSS-stle hex color (with #) to form needed by syntax highlighting.
  // Add a JS color display extension to have colors displayed in source
  function cs(color) {
    return color.substr(1);
  }

  var base03 = '#002b36';
  var base02 = '#073642';
  var base01 = '#586e75';
  var base00 = '#657b83';
  var base0 = '#839496';
  var base1 = '#93a1a1';
  var base2 = '#eee8d5';
  var base3 = '#fdf6e3';
  var yellow = '#b58900';
  var orange = '#cb4b16';
  var red = '#dc322f';
  var magenta = '#d33682';
  var violet = '#6c71c4';
  var blue = '#268bd2';
  var cyan = '#2aa198';
  var green = '#859900';

  monaco.editor.defineTheme('one-light-pro', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#303030') },
      { token: 'keyword', foreground: cs('#1010a0') },
      { token: 'symbols', foreground: cs('#303030') },
      { token: 'comment', foreground: cs('#308030') },
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#ff0000') },
      { token: 'string', foreground: cs('#e06c75') },
      { token: 'number.bare', foreground: cs("#c08000") },
      { token: 'number.hash', foreground: cs("#408080") }
    ],
    "colors": {
      'editor.foreground': '#000000',
      'editor.background': '#EDF9FA',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base3
    }
  });

  monaco.editor.defineTheme('one-dark-pro', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#b0b0b0') },
      { token: 'keyword', foreground: cs(blue) },
      { token: 'symbol', foreground: cs('#a0a0a0') },
      { token: 'comment', foreground: cs('#20a020') },
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2') },
      { token: 'string', foreground: cs('#e06c75') },
      { token: 'number.hash', foreground: cs("#80c0c0") },
      { token: 'number.bare', foreground: cs("#f0f080") }
    ],
    "colors": {
      'editor.foreground': '#FFFFFF',
      //'editor.background': '#000000',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base02,
      'editorLineNumber.foreground': base01,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base01,
      'editor.findMatchBackground': base00, // Color of the current search match.
      'editor.findMatchHighlightBackground': base02 // Color of the other search matches.
    }
  });


  monaco.editor.defineTheme('solarised-light', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'delimiter', foreground: cs(base00) },
      { token: 'identifier', foreground: cs(base00) },
      { token: 'keyword', foreground: cs(blue) },
      { token: 'symbol', foreground: cs(base00) },
      { token: 'comment', foreground: cs(green) },
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2') },
      { token: 'string', foreground: cs('#e06c75') },
      { token: 'number.hash', foreground: cs(cyan) },
      { token: 'number.bare', foreground: cs(yellow) }
    ],
    "colors": {
      'foreground': base00,
      'editor.foreground': base00,
      'editor.background': base3,
      'editorCursor.foreground': magenta,
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': base1,
      'editor.inactiveSelectionBackground': base1,
      'editor.findMatchBackground': base0, // Color of the current search match.
      'editor.findMatchHighlightBackground': base2 // Color of the other search matches.
    }
  });


  monaco.editor.defineTheme('solarised-dark', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'delimiter', foreground: cs(base0) },
      { token: 'identifier', foreground: cs(base0) },
      { token: 'keyword', foreground: cs(blue) },
      { token: 'symbol', foreground: cs(base0) },
      { token: 'comment', foreground: cs(green) },
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2') },
      { token: 'string', foreground: cs('#e06c75') },
      { token: 'number.hash', foreground: cs(cyan) },
      { token: 'number.bare', foreground: cs(yellow) }
    ],
    "colors": {
      'foreground': base0,
      'editor.foreground': base0,
      'editor.background': base03,
      'editorCursor.foreground': magenta,
      'editor.lineHighlightBackground': base02,
      'editorLineNumber.foreground': base01,
      'editor.selectionBackground': base01,
      'editor.inactiveSelectionBackground': base01,
      'editor.findMatchBackground': base00, // Color of the current search match.
      'editor.findMatchHighlightBackground': base02 // Color of the other search matches.
    }
  });


  //'foreground' // Overall foreground color. This color is only used if not overridden by a component.
  //'errorForeground' // Overall foreground color for error messages. This color is only used if not overridden by a component.
  //'descriptionForeground' // Foreground color for description text providing additional information, for example for a label.
  //'focusBorder' // Overall border color for focused elements. This color is only used if not overridden by a component.
  //'contrastBorder' // An extra border around elements to separate them from others for greater contrast.
  //'contrastActiveBorder' // An extra border around active elements to separate them from others for greater contrast.
  //'selection.background' // The background color of text selections in the workbench (e.g. for input fields or text areas). Note that this does not apply to selections within the editor.
  //'textSeparator.foreground' // Color for text separators.
  //'textLink.foreground' // Foreground color for links in text.
  //'textLink.activeForeground' // Foreground color for active links in text.
  //'textPreformat.foreground' // Foreground color for preformatted text segments.
  //'textBlockQuote.background' // Background color for block quotes in text.
  //'textBlockQuote.border' // Border color for block quotes in text.
  //'textCodeBlock.background' // Background color for code blocks in text.
  //'widget.shadow' // Shadow color of widgets such as find/replace inside the editor.
  //'input.background' // Input box background.
  //'input.foreground' // Input box foreground.
  //'input.border' // Input box border.
  //'inputOption.activeBorder' // Border color of activated options in input fields.
  //'input.placeholderForeground' // Input box foreground color for placeholder text.
  //'inputValidation.infoBackground' // Input validation background color for information severity.
  //'inputValidation.infoBorder' // Input validation border color for information severity.
  //'inputValidation.warningBackground' // Input validation background color for information warning.
  //'inputValidation.warningBorder' // Input validation border color for warning severity.
  //'inputValidation.errorBackground' // Input validation background color for error severity.
  //'inputValidation.errorBorder' // Input validation border color for error severity.
  //'dropdown.background' // Dropdown background.
  //'dropdown.foreground' // Dropdown foreground.
  //'dropdown.border' // Dropdown border.
  //'list.focusBackground' // List/Tree background color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
  //'list.focusForeground' // List/Tree foreground color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
  //'list.activeSelectionBackground' // List/Tree background color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
  //'list.activeSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
  //'list.inactiveSelectionBackground' // List/Tree background color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
  //'list.inactiveSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
  //'list.hoverBackground' // List/Tree background when hovering over items using the mouse.
  //'list.hoverForeground' // List/Tree foreground when hovering over items using the mouse.
  //'list.dropBackground' // List/Tree drag and drop background when moving items around using the mouse.
  //'list.highlightForeground' // List/Tree foreground color of the match highlights when searching inside the list/tree.
  //'pickerGroup.foreground' // Quick picker color for grouping labels.
  //'pickerGroup.border' // Quick picker color for grouping borders.
  //'button.foreground' // Button foreground color.
  //'button.background' // Button background color.
  //'button.hoverBackground' // Button background color when hovering.
  //'badge.background' // Badge background color. Badges are small information labels, e.g. for search results count.
  //'badge.foreground' // Badge foreground color. Badges are small information labels, e.g. for search results count.
  //'scrollbar.shadow' // Scrollbar shadow to indicate that the view is scrolled.
  //'scrollbarSlider.background' // Slider background color.
  //'scrollbarSlider.hoverBackground' // Slider background color when hovering.
  //'scrollbarSlider.activeBackground' // Slider background color when active.
  //'progressBar.background' // Background color of the progress bar that can show for long running operations.
  //'editor.background' // Editor background color.
  //'editor.foreground' // Editor default foreground color.
  //'editorWidget.background' // Background color of editor widgets, such as find/replace.
  //'editorWidget.border' // Border color of editor widgets. The color is only used if the widget chooses to have a border and if the color is not overridden by a widget.
  //'editor.selectionBackground' // Color of the editor selection.
  //'editor.selectionForeground' // Color of the selected text for high contrast.
  //'editor.inactiveSelectionBackground' // Color of the selection in an inactive editor.
  //'editor.selectionHighlightBackground' // Color for regions with the same content as the selection.
  //'editor.findRangeHighlightBackground' // Color the range limiting the search.
  //'editor.hoverHighlightBackground' // Highlight below the word for which a hover is shown.
  //'editorHoverWidget.background' // Background color of the editor hover.
  //'editorHoverWidget.border' // Border color of the editor hover.
  //'editorLink.activeForeground' // Color of active links.
  //'diffEditor.insertedTextBackground' // Background color for text that got inserted.
  //'diffEditor.removedTextBackground' // Background color for text that got removed.
  //'diffEditor.insertedTextBorder' // Outline color for the text that got inserted.
  //'diffEditor.removedTextBorder' // Outline color for text that got removed.
  //'merge.currentHeaderBackground' // Current header background in inline merge-conflicts.
  //'merge.currentContentBackground' // Current content background in inline merge-conflicts.
  //'merge.incomingHeaderBackground' // Incoming header background in inline merge-conflicts.
  //'merge.incomingContentBackground' // Incoming content background in inline merge-conflicts.
  //'merge.commonHeaderBackground' // Common ancestor header background in inline merge-conflicts.
  //'merge.commonContentBackground' // Common ancester content background in inline merge-conflicts.
  //'merge.border' // Border color on headers and the splitter in inline merge-conflicts.
  //'editorOverviewRuler.currentContentForeground' // Current overview ruler foreground for inline merge-conflicts.
  //'editorOverviewRuler.incomingContentForeground' // Incoming overview ruler foreground for inline merge-conflicts.
  //'editorOverviewRuler.commonContentForeground' // Common ancestor overview ruler foreground for inline merge-conflicts.
  //'editor.lineHighlightBackground' // Background color for the highlight of line at the cursor position.
  //'editor.lineHighlightBorder' // Background color for the border around the line at the cursor position.
  //'editor.rangeHighlightBackground' // Background color of highlighted ranges, like by quick open and find features.
  //'editorCursor.foreground' // Color of the editor cursor.
  //'editorWhitespace.foreground' // Color of whitespace characters in the editor.
  //'editorIndentGuide.background' // Color of the editor indentation guides.
  //'editorLineNumber.foreground' // Color of editor line numbers.
  //'editorRuler.foreground' // Color of the editor rulers.
  //'editorCodeLens.foreground' // Foreground color of editor code lenses
  //'editorBracketMatch.background' // Background color behind matching brackets
  //'editorBracketMatch.border' // Color for matching brackets boxes
  //'editorOverviewRuler.border' // Color of the overview ruler border.
  //'editorGutter.background' // Background color of the editor gutter. The gutter contains the glyph margins and the line numbers.
  //'editorError.foreground' // Foreground color of error squigglies in the editor.
  //'editorError.border' // Border color of error squigglies in the editor.



  var mevent = new CustomEvent("monaco-ready", { "detail": "ready now!" });

  // Dispatch/Trigger/Fire the event
  document.dispatchEvent(mevent);
});






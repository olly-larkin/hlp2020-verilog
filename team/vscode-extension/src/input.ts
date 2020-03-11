import * as vscode from 'vscode';

export const getInput = (prompt: string, validator: any) => vscode.window.showInputBox({
    ignoreFocusOut: true,
    prompt: prompt,
    validateInput: validator,
});
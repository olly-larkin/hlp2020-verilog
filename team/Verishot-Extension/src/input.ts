import * as vscode from 'vscode';

export const getInput = (prompt: string, validator: any) => vscode.window.showInputBox({
    ignoreFocusOut: true,
    prompt: prompt,
    validateInput: validator,
});

export const getPick = (items: any, placeHolder: string) => vscode.window.showQuickPick(items, {
    canPickMany: false,
    ignoreFocusOut: true,
    placeHolder: placeHolder,
});
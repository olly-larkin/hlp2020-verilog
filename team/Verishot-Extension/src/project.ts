import { getProjectName, getWorkspacePath, spawnCmdWithFeedback, getExistingModules } from './utility';
import { getInput, getPick } from './input';
import * as path from 'path';
import * as vscode from 'vscode';

export const newProjectHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    getInput(`Enter project name`,
        (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text) && text.length) { return undefined; } return `Enter a valid project name`; })
        .then((projectName) => {
            if (projectName) {
                const moduleFilePath = path.join(workspacePath, `${projectName}.v`);
                const succFunc = () => { vscode.workspace.openTextDocument(moduleFilePath).then(doc => vscode.window.showTextDocument(doc)); };
                spawnCmdWithFeedback(`verishot`, [`--new-project`, workspacePath, projectName], succFunc);
            }
        });
};

export const newModuleHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    const projectName = getProjectName(workspacePath);
    if (!projectName) { return; }
    const vprojFilePath = path.join(workspacePath, `${projectName}.vproj`);
    getInput(`Enter module name`,
        (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text)) { return undefined; } return `Enter a valid module name`; })
        .then((moduleName) => {
            if (moduleName && projectName) {
                const moduleFilePath = path.join(workspacePath, `${moduleName}.v`);
                const succFunc = () => { vscode.workspace.openTextDocument(moduleFilePath).then(doc => vscode.window.showTextDocument(doc)); };
                spawnCmdWithFeedback(`verishot`, [`--new-module`, vprojFilePath, moduleName], succFunc);
            }
        });
};

export const deleteModuleHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    const projectName = getProjectName(workspacePath);
    if (!projectName) { return; }
    const vprojFilePath = path.join(workspacePath, `${projectName}.vproj`);
    const existingModules = getExistingModules(workspacePath, projectName);
    getPick(existingModules, `Select module to delete`).then((moduleName) => {
        if (moduleName) {
            spawnCmdWithFeedback(`verishot`, [`--delete-module`, vprojFilePath, moduleName]);
        }
    });
};
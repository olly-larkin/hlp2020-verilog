import * as vscode from 'vscode';
import { getNumberOfProjectFiles, dirSlash, getProjectName } from './utility';
import { getInput } from './input';
import * as fs from 'fs';

// workspacePath has no / at the end
const createNewProject = (projectName: string, workspacePath: string) => {
    if (!getNumberOfProjectFiles(workspacePath)) {
        createNewModule(projectName, workspacePath, projectName);
    }
    else {
        vscode.window.showErrorMessage("This workspace already contains an existing project.");
    }
};

const getModuleTemplate = (moduleName: string) => {
    return `module ${moduleName}(\n);\nendmodule`;
}

const createNewModule = (moduleName: string, workspacePath: string, projectName: string) => {
    const numberOfProjectFiles = getNumberOfProjectFiles(workspacePath);
    if (numberOfProjectFiles > 1) {
        vscode.window.showErrorMessage("More than one project file exists.");
    }
    else {
        const moduleFileName = `${moduleName}.v`;

        // add to .vproj
        try {
            const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
            const vprojLines = fs.existsSync(vprojFilePath) ?
                fs.readFileSync(vprojFilePath, 'utf-8')
                    .split('\n')
                    .filter(Boolean)
                :
                [];
            const lastLine = vprojLines.pop();
            vprojLines.push(moduleFileName);
            if (lastLine) { vprojLines.push(lastLine); }
            const vprojContent = vprojLines.join('\n');
            fs.writeFileSync(vprojFilePath, vprojContent);

        }
        catch (err) {
            vscode.window.showErrorMessage(err.message);
        }

        // create the new module
        try {
            const moduleTemplateContent = getModuleTemplate(moduleName);
            const moduleFilePath = `${workspacePath}${dirSlash}${moduleName}.v`;
            fs.writeFileSync(moduleFilePath, moduleTemplateContent);
        }
        catch (err) {
            vscode.window.showErrorMessage(err.message);
        }

    }
};

export const newProjectHandler = () => {
    const folders = vscode.workspace.workspaceFolders || [];
    if (folders.length === 1) {
        const workspacePath = folders[0].uri.fsPath;
        getInput(`Enter project name`,
            (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text) && text.length) { return undefined; } return `Enter a valid project name`; })
            .then((projectName) => {
                if (projectName) {
                    createNewProject(projectName, workspacePath);
                }
                else {
                    vscode.window.showErrorMessage(`Enter a valid project name.`);
                }
            });
    }
    else {
        vscode.window.showErrorMessage(`Verishot only supports 1 project folder.`);
    }
};

export const newModuleHandler = () => {
    const folders = vscode.workspace.workspaceFolders || [];
    if (folders.length === 1) {
        const workspacePath = folders[0].uri.fsPath;
        const projectName = getProjectName(workspacePath);
        if (projectName) {
            getInput(`Enter module name`,
                (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text)) { return undefined; } return `Enter a valid module name`; })
                .then((moduleName) => {
                    if (moduleName && projectName) {
                        createNewModule(moduleName, workspacePath, projectName);
                    }
                    else {
                        vscode.window.showErrorMessage(`Enter a valid module name`);
                    }
                });
        }
        else {
            if (!getNumberOfProjectFiles(workspacePath)) {
                vscode.window.showErrorMessage(`Create a new project first`);
            }
            else {
                vscode.window.showErrorMessage(`More than 1 project files found`);
            }
        }
    }
    else {
        vscode.window.showErrorMessage(`Verishot only supports 1 project folder.`);
    }
};
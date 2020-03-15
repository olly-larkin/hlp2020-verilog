import * as vscode from 'vscode';
import { getExistingModuleNamesFromVProj, getNumberOfProjectFiles, dirSlash, getProjectName, readLinesFromFile, getWorkspacePath, deleteFileIfExists } from './utility';
import { getInput, getPick } from './input';
import * as fs from 'fs';



// workspacePath has no / at the end
const createNewProject = (projectName: string, workspacePath: string) => {
    if (!getNumberOfProjectFiles(workspacePath)) {
        createNewModule(projectName, workspacePath, projectName);
    }
    else {
        vscode.window.showErrorMessage(`This workspace already contains an existing project.`);
    }
};

const getModuleTemplate = (moduleName: string) => {
    return `module ${moduleName}(\n);\nendmodule`;
};

const createNewModule = (moduleName: string, workspacePath: string, projectName: string) => {
    const numberOfProjectFiles = getNumberOfProjectFiles(workspacePath);
    if (numberOfProjectFiles > 1) {
        vscode.window.showErrorMessage("More than one project file exists.");
    }
    else if (getExistingModuleNamesFromVProj(workspacePath, projectName).includes(`${moduleName}`)) {
        vscode.window.showErrorMessage(`Module already exists. Please use the \`Verishot: Delete module\` command to delete it.`);
    }
    else {
        const moduleFileName = `${moduleName}.v`;

        try {
            // add to .vproj
            const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
            const vprojLines = readLinesFromFile(vprojFilePath);
            const lastLine = vprojLines.pop();
            vprojLines.push(moduleFileName);
            if (lastLine) { vprojLines.push(lastLine); }
            const vprojContent = vprojLines.join('\n');
            fs.writeFileSync(vprojFilePath, vprojContent);

            // create the new module
            const moduleTemplateContent = getModuleTemplate(moduleName);
            const moduleFilePath = `${workspacePath}${dirSlash}${moduleName}.v`;
            fs.writeFileSync(moduleFilePath, moduleTemplateContent);
            
            vscode.workspace.openTextDocument(moduleFilePath).then(doc => vscode.window.showTextDocument(doc));
            console.log("opening!");
        }
        catch (err) {
            vscode.window.showErrorMessage(err.message);
        }
    }
};

const deleteModule = (moduleName: string, workspacePath: string, projectName: string) => {
    try {    
        // update vproj
        const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
        const vprojLines = readLinesFromFile(vprojFilePath);
        const newvprojLines = vprojLines.filter((line: string) => line !== `${moduleName}.v`);
        const vprojContent = newvprojLines.join('\n');
        fs.writeFileSync(vprojFilePath, vprojContent);

        // delete the file
        const moduleFilePath = `${workspacePath}${dirSlash}${moduleName}.v`;
        deleteFileIfExists(moduleFilePath);
    }
    catch (err) {
        vscode.window.showErrorMessage(err.message);
    }
};

export const newProjectHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    getInput(`Enter project name`,
        (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text) && text.length) { return undefined; } return `Enter a valid project name`; })
        .then((projectName) => {
            if (projectName) {
                createNewProject(projectName, workspacePath);
            }
        });
};

export const newModuleHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    const projectName = getProjectName(workspacePath);
    if (!projectName) { return; }
    getInput(`Enter module name`,
        (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text)) { return undefined; } return `Enter a valid module name`; })
        .then((moduleName) => {
            if (moduleName && projectName) {
                createNewModule(moduleName, workspacePath, projectName);
            }
        });
};

export const deleteModuleHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    const projectName = getProjectName(workspacePath);
    if (!projectName) { return; }
    const allModulesWithTopLevelModule = getExistingModuleNamesFromVProj(workspacePath, projectName);
    const allModules = allModulesWithTopLevelModule.filter((moduleName: string) => moduleName !== `${projectName}`);
    getPick(allModules, `Select module to delete`).then((moduleName) => {
        if (moduleName) {
            deleteModule(moduleName, workspacePath, projectName);
        }
    });
};
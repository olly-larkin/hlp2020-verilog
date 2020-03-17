import { getProjectName, getWorkspacePath, spawnCmdWithFeedback, getExistingModules } from './utility';
import { getInput, getPick } from './input';


export const newProjectHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    getInput(`Enter project name`,
        (text: string) => { if (/[a-zA-Z][a-zA-Z0-9_]*/.test(text) && text.length) { return undefined; } return `Enter a valid project name`; })
        .then((projectName) => {
            if (projectName) {
                spawnCmdWithFeedback(`verishot`, [`--new-project`, workspacePath, projectName]);
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
                spawnCmdWithFeedback(`verishot`, [`--new-module`, workspacePath, projectName, moduleName]);
            }
        });
};

export const deleteModuleHandler = () => {
    const workspacePath = getWorkspacePath();
    if (!workspacePath) { return; }
    const projectName = getProjectName(workspacePath);
    if (!projectName) { return; }
    const existingModules = getExistingModules(workspacePath, projectName);
    getPick(existingModules, `Select module to delete`).then((moduleName) => {
        if (moduleName) {
            spawnCmdWithFeedback(`verishot`, [`--delete-module`, workspacePath, projectName, moduleName]);
        }
    });
};
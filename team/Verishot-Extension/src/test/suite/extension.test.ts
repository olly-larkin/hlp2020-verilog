import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { checkFilePath, getFileExtension, stripFileExtension, getWorkspacePath, getNumberOfProjectFiles, getProjectName, getExistingModules } from '../../utility';


suite('Utility Test Suite', () => {
	vscode.window.showInformationMessage('Start `utility` tests.');

	test('getFileExtensionTests', () => {
		assert.equal(getFileExtension(`abc/abc.v`), `v`);
		assert.equal(getFileExtension(`abc/abc.abc`), `abc`);
		assert.equal(getFileExtension(`abc.abc`), `abc`);
		assert.equal(getFileExtension(`abc`), `abc`);
	});

	test(`stripFileExtensionTests`, () => {
		assert.equal(stripFileExtension(`abc.v`), `abc`);
		assert.equal(stripFileExtension(`abc.def`), `abc`);
		assert.equal(stripFileExtension(`abc`), `abc`);
	});

	test('checkFilePathTests', () => {
		assert.equal(checkFilePath(undefined, true), false);
		assert.equal(checkFilePath("ers/fdaf/fdar.v", true), true);
		assert.equal(checkFilePath("fdasf.fdfad.vv", true), false);
	});
});

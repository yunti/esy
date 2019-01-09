// @flow

const child_process = require('child_process');
const os = require('os');
const fs = require('fs-extra');
const path = require('path');
const outdent = require('outdent');

const isWindows = os.platform() === 'win32';

const root = path.normalize(path.join(__dirname, '..'));
const bin = path.join(root, 'bin');

fs.mkdirpSync(bin);

function which(cmd) {
  const which = isWindows ? 'C:\\Windows\\System32\\WHERE' : 'which';
  return child_process
    .execSync(`${which} esy-solve-cudf`)
    .toString()
    .trim();
}

const esyBashPath = path.dirname(require.resolve('esy-bash/package.json'));
const esySolveCudf = path.dirname(require.resolve('esy-solve-cudf/package.json'));
const esySolveCudfExe = path.join(esySolveCudf, 'esySolveCudfCommand.exe');

if (isWindows) {
  {
    const p = path.join(bin, 'esy.cmd');
    fs.writeFileSync(
      p,
      outdent`
      @ECHO off
      @SETLOCAL
      @SET ESY__SOLVE_CUDF_COMMAND=${esySolveCudfExe}
      @SET ESY__ESY_BASH=${esyBashPath}
      "${root}/_build/default/bin/esy.exe" %*
      `
    );
  }
  {
    const p = path.join(bin, 'esydev.cmd');
    fs.writeFileSync(
      p,
      outdent`
      @ECHO off
      @SETLOCAL
      @SET ESY__SOLVE_CUDF_COMMAND=${esySolveCudfExe}
      @SET ESY__ESY_BASH=${esyBashPath}
      esy %*
      `
    );
  }
} else {
  {
    const p = path.join(bin, 'esy');
    fs.writeFileSync(
      p,
      outdent`
      #!/bin/bash
      export ESY__SOLVE_CUDF_COMMAND="${esySolveCudfExe}"
      export ESY__ESY_BASH="${esyBashPath}"
      exec "${root}/_build/default/bin/esy.exe" "$@"
      `
    );
    fs.chmodSync(p, 0o755);
}
  {
    const p = path.join(bin, 'esydev');
    fs.writeFileSync(
      p,
      outdent`
      #!/bin/bash
      export ESY__SOLVE_CUDF_COMMAND="${esySolveCudfExe}"
      export ESY__ESY_BASH="${esyBashPath}"
      exec esy "$@"
      `
    );
    fs.chmodSync(p, 0o755);
  }
}

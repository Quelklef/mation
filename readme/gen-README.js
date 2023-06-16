const fs = require('fs');
const plib = require('path');

main();

function main() {

  let readmeText = fs.readFileSync(plib.join(__dirname, './README.template.md')).toString();
  const sampleText = fs.readFileSync(plib.join(__dirname, '../samples/Counter.purs')).toString();

  function wellFormed(text, markerBegin, markerEnd) {
    return (
      count(text, markerBegin) === 1
      && count(text, markerEnd) === 1
      && text.indexOf(markerBegin) <= text.indexOf(markerEnd)
    );
  }

  if (!wellFormed(sampleText, 'SAMPLE_BEGIN', 'SAMPLE_END')) throw Error('Malformed sample');

  const readmeLines = readmeText.split('\n');
  const readmeIdx = indexOf(readmeLines, line => line.includes('SAMPLE_HERE'));

  const sampleLines = sampleText.split('\n');
  const sampleFrom = indexOf(sampleLines, line => line.includes('SAMPLE_BEGIN'));
  const sampleTo = indexOf(sampleLines, line => line.includes('SAMPLE_END'));

  const resultText = (
    [].concat(
        readmeLines.slice(0, readmeIdx),
        ['```purescript'],
        (sampleLines
          .slice(sampleFrom + 1, sampleTo)
          .map(line => line.includes('module ') ? 'module Main where' : line)),
        ['```'],
        readmeLines.slice(readmeIdx + 1),
    )
    .join('\n')
  )

  fs.writeFileSync(plib.join(__dirname, '../README.md'), resultText);
  console.log('README generated');

}

function count(str, substr) {
  return str.split(substr).length - 1;
}

function indexOf(arr, pred) {
  for (let i = 0; i < arr.length; i++)
    if (pred(arr[i]))
      return i;
  return -1;
}


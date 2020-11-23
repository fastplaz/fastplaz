const fs = require('fs');
const path = require('path');

const main = async () => {
  try {
    
    // do something

  } catch (error) {
    console.error('❌ Error execute updater.js', error);
  }
}

function doDashes(str) {
  return str.replace(/[^a-z0-9]+/gi, '-').replace(/^-*|-*$/g, '').toLowerCase();
}

main();

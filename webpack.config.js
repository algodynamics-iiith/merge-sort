const path = require('path');

module.exports = {
  entry: {
    mergeSortRecursiveInit: './src/mergeSortRecursiveInit.js',
    msArbitraryInit: './src/msArbitraryInit.js'
    
  },
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist', 'js'),
  },
};

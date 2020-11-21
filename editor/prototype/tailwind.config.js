module.exports = {
  purge: [],
  darkMode: 'media', // or 'media' or 'class'
  theme: {
    extend: {
      fontFamily: {
        mono : ['JetBrains Mono', 'monospace']
      } 
    },
  },
  variants: {
    extend: {
      backgroundColor: ['group-hover']
    },
  },
  plugins: [],
}

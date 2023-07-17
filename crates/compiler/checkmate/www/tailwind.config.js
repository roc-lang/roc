/** @type {import('tailwindcss').Config} */
module.exports = {
  mode: 'jit',
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        'roc-purple': '#7c38f5',
        'roc-purple-bg': '#ece2fd',
      },
    },
  },
  plugins: [],
}


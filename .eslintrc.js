module.exports = {
    "env": {
        "browser": true
    },
    "extends": "eslint:recommended",
    "parserOptions": {
        "ecmaVersion": 5
    },
    "globals": {
        "$": "readable",
        "L": "readable",
        "Cookies": "readable",
        "ArrayBuffer": "readable",
        "Uint8Array": "readable",
    },
    "rules": {
        "indent": [
            "error",
            2
        ],
        "linebreak-style": [
            "error",
            "unix"
        ],
        "quotes": [
            "error",
            "single"
        ],
        "no-empty": [
            "error",
            {
                "allowEmptyCatch": true
            }
        ],
        "semi": [
            "error",
            "never"
        ]
    }
};

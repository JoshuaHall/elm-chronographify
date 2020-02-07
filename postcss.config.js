module.exports = {
    plugins: [
        require("stylelint")({
            config: {
                extends: "stylelint-config-standard"
            }
        }),
        require("autoprefixer"),
        require("cssnano")({
            preset: "default"
        })
    ]
};

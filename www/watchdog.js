/*
    Watchdog to check whether the WASM module could be executed by the browser.

    Uses classic JavaScript syntax (no module) for better browser compatibility.
*/
(function () {
    function start() {
        console.debug("Application watchdog started");
        window.setTimeout(stop, 8*1000); /* 8 seconds */
    }

    function stop() {
        console.debug("Application watchdog stopped");
        const tiles = document.querySelectorAll('#catalog .tile') || [];
        console.debug("number of catalog tiles: " + tiles.length);
        const map = document.querySelector('#map');
        console.debug("map element: " + (!!map ? "found" : "not found"));
        if (tiles.length == 0 || !map) {
            /* either the catalog tiles or the map could not be loaded, activate the warning message */
            const element = document.querySelector('#watchdog-info');
            element.classList.add('is-active');
        }
    }

    start();
})();

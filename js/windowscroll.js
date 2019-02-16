function windowScrollPort(elmApp) {
    window.addEventListener("scroll", ev => {
        let scrollTop = window.scrollY || window.pageYOffset || document.body.scrollTop + (document.documentElement && document.documentElement.scrollTop || 0)
        let scrollLeft = window.scrollX || window.pageXOffset || document.body.scrollLeft + (document.documentElement && document.documentElement.scrollLeft || 0)
        let scrollHeight = document.documentElement.offsetHeight
        let offsetHeight = window.innerHeight
        let windowScrollObj = {
                scrollTop: scrollTop,
                scrollHeight: scrollHeight,
                offsetHeight: offsetHeight,
                clientHeight: offsetHeight
            }

        elmApp.ports.windowScroll.send({
            target : windowScrollObj
        })
    })
}

exports.init = function(app) {
    if (window.self !== window.top) {
        app.ports.isIFrameTestPort.send(JSON.stringify({ isInIframe : true }));
        console.log("This page is inside an iframe.");
    } else {
        app.ports.isIFrameTestPort.send(JSON.stringify({ isInIframe : false }));
        console.log("This page is not inside an iframe.");
    }

    window.addEventListener("message", (event) => {
        window.parent.postMessage(JSON.stringify(event.data), "*");
        app.ports.fromParentPort.send(event.data);
        console.log("Message from host:", event.data);
    });

    // app.ports.getGeoloc.subscribe(async function() {
    //     getLocation(app)
    // })
}

// function getLocation(app) {
//     var options = {
//         enableHighAccuracy : true,
//         timeout : 5000,
//         maximumAge : 0
//     };

//     if (navigator.geolocation) {
//         navigator.geolocation.getCurrentPosition(app.ports.geoloc.send,
//             function(error) {
//                 app.ports.geoloc.send("geolocation not allowed");
//             },
//             options);
//     } else {
//         app.ports.geoloc.send("no geolocation");
//         console.log("no geolocation");
//     }
// }
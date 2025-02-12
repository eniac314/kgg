exports.init = function(app) {
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
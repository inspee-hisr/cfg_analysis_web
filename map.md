---
title: Interactive Map
excerpt: "This map was created using Leaflet js application"
layout: page

---

<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Leaflet Map</title>
    <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" />
    <style>
        #map {
            height: 500px;
            width: 100%;
        }
    </style>
</head>
<body>

    <div id="map"></div>

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
    var map = L.map('map').setView([37.9286, 24.7956], 5);

    // Add OpenStreetMap layer
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; OpenStreetMap contributors'
    }).addTo(map);

    // Load external GeoJSON file
    fetch('../assets/CFG_map/caves.geojson')
        .then(response => response.json())
        .then(data => {
            L.geoJSON(data, {
                onEachFeature: function (feature, layer) {
                    if (feature.properties) {
                        let popupContent = `<b>${feature.properties.Cave_Name}</b><br>
                                            <b>Type:</b> ${feature.properties.Cave_Type}<br>
                                            <b>Protection:</b> ${feature.properties.Protection_Status}<br>
                                            <b>Description:</b> ${feature.properties.Cave_Description}`;

                        layer.bindPopup(popupContent);
                    }
                }
            }).addTo(map);
        })
        .catch(error => console.log("Error loading GeoJSON points: ", error));
</script>

</body>
</html>

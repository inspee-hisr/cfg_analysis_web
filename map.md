---
title: Map
excerpt: "This map was created using Shiny application"
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

    <h1>Interactive Map</h1>
    <div id="map"></div>

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
    var map = L.map('map').setView([40.7128, -74.0060], 5);

    // ✅ Add OpenStreetMap layer
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; OpenStreetMap contributors'
    }).addTo(map);

    // ✅ Load external GeoJSON file
    fetch('../assets/CFG_map/points.geojson')
        .then(response => response.json())
        .then(data => {
            L.geoJSON(data, {
                onEachFeature: function (feature, layer) {
                    if (feature.properties) {
                        layer.bindPopup("<b>" + feature.properties.name + "</b><br>" + feature.properties.description);
                    }
                }
            }).addTo(map);
        })
        .catch(error => console.log("Error loading GeoJSON: ", error));
</script>

</body>
</html>

---
title: Map
excerpt: "This map was created using Shiny application"
layout: page

---
## Map with Multiple Layers

<div id="map" style="height: 500px; width: 100%;"></div>

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
    var map = L.map('map').setView([40.7128, -74.0060], 5);

    // Define base layers
    var osm = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', { attribution: '&copy; OpenStreetMap' });
    var satellite = L.tileLayer('https://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}', {
        attribution: '© Google',
        subdomains: ['mt0', 'mt1', 'mt2', 'mt3']
    });

    osm.addTo(map); // Default layer

    // Load external point data
    var pointsLayer = new L.LayerGroup();
    fetch('assets/CFG_map/points.geojson')
        .then(response => response.json())
        .then(data => {
            L.geoJSON(data, {
                onEachFeature: function(feature, layer) {
                    if (feature.properties) {
                        layer.bindPopup("<b>" + feature.properties.name + "</b><br>" + feature.properties.description);
                    }
                }
            }).addTo(pointsLayer);
        });

    // Add a polygon layer (example)
    var polygonLayer = L.geoJSON({
        "type": "Feature",
        "geometry": { "type": "Polygon", "coordinates": [[[-75, 40], [-75, 42], [-72, 42], [-72, 40], [-75, 40]]] },
        "properties": { "name": "Example Area" }
    }).bindPopup("This is an example polygon.");

    // Layer control
    L.control.layers(
        { "OpenStreetMap": osm, "Satellite": satellite },
        { "Points": pointsLayer, "Polygon": polygonLayer }
    ).addTo(map);
</script>

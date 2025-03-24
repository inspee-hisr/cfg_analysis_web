---
title: Map
excerpt: "This map was created using Shiny application"
layout: page

---
## Interactive Map

<div id="map" style="height: 500px; width: 100%;"></div>

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
    var map = L.map('map').setView([51.505, -0.09], 13);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; OpenStreetMap contributors'
    }).addTo(map);
    L.marker([51.505, -0.09]).addTo(map)
        .bindPopup("Hello! This is a Leaflet map.")
        .openPopup();
</script>
#<div class="container"> 
#  <iframe src='https://savvas-paragkamian.shinyapps.io/Spatial_caves_CFG/'></iframe>
#</div>

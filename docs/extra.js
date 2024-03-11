window.onload = init;

function init() {

const mapElement = document.getElementById("mapid");

var box = [-117.06278, 45.22677, -114.29213, 48.99998];

// define rectangle geographical bounds
var bounds = [[box[1], box[0]], [box[3], box[2]]];


var map = L.map(mapElement).fitBounds(bounds);

  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
  }).addTo(map);



$.getJSON("final_lakes.geojson", function(data) {

  L.geoJSON(data, {

  style:
    {'color': '#000',
    'fillOpacity': 0.8,
    'opacity': 0.5,
    'weight':1},
  onEachFeature: function onEachFeature(feature, layer) {
                layer.bindPopup('<strong>' + feature.properties.huc12 + '</strong><br><br>Final Score: ' + feature.properties.final_score);
                switch(feature.properties.final_score){

  case 1: return layer.setStyle({fillColor: "#0D0887FF"});
  case 2: return layer.setStyle({fillColor: "#47039FFF"});
  case 3: return layer.setStyle({fillColor: "#7301A8FF"});
  case 4: return layer.setStyle({fillColor: "#9C179EFF"});
  case 5: return layer.setStyle({fillColor: "#BD3786FF"});
  case 6: return layer.setStyle({fillColor:  "#D8576BFF"});
  case 7: return layer.setStyle({fillColor: "#ED7953FF"});
  case 8: return layer.setStyle({fillColor:  "#FA9E3BFF" });
  case 9: return layer.setStyle({fillColor: "#FDC926FF"});
  case 10: return layer.setStyle({fillColor: "#F0F921FF"});
}
        }

  }).addTo(map);

});


$.getJSON("monitoring.geojson", function(data) {

  L.geoJSON(data).addTo(map);

});

$.getJSON("final_streams.geojson", function(data) {

  L.geoJSON(data,{

  style:
    {'weight':1},
  onEachFeature: function onEachFeature(feature, layer) {
                layer.bindPopup('<strong>' + feature.properties.huc12 + '</strong><br><br>Final Score: ' + feature.properties.final_score);
                switch(feature.properties.final_score){

  case 1: return layer.setStyle({color: "#0D0887FF"});
  case 2: return layer.setStyle({color: "#47039FFF"});
  case 3: return layer.setStyle({color: "#7301A8FF"});
  case 4: return layer.setStyle({color: "#9C179EFF"});
  case 5: return layer.setStyle({color: "#BD3786FF"});
  case 6: return layer.setStyle({color:  "#D8576BFF"});
  case 7: return layer.setStyle({color: "#ED7953FF"});
  case 8: return layer.setStyle({color:  "#FA9E3BFF" });
  case 9: return layer.setStyle({color: "#FDC926FF"});
  case 10: return layer.setStyle({color: "#F0F921FF"});
}
        }

  }).addTo(map);

});

var legend = L.control({position: 'bottomright'});

legend.onAdd = function (map) {

    var div = L.DomUtil.create('div', 'info legend'),
    grades = [0, 10, 20, 50, 100, 200, 500, 1000],
    labels = [];

    // loop through our density intervals and generate a label with a colored square for each interval
    for (var i = 0; i < grades.length; i++) {
        div.innerHTML +=
            '<i style="background:' + getColor(grades[i] + 1) + '"></i> ' +
            grades[i] + (grades[i + 1] ? '&ndash;' + grades[i + 1] + '<br>' : '+');
}

return div;
};


legend.addTo(map);

}


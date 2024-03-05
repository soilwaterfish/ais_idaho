window.onload = init;

function init() {

const mapElement = document.getElementById("mapid");

var map = L.map(mapElement).setView([48.5, -114.165], 8);

  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
  }).addTo(map);



}

  $.getJSON("final_lakes.geojson", function(data) {
  var final_score = L.geoJSON(data,
            {style: function(feature) {
              switch (feature.properties.final_score) {
                case '1': return {color: "#32CD32"};
                case '2': return {color: "#C51C0F"};
                case '3': return {color: "#1E90FF"};
                case '4': return {color: "#314004"};
                case '5': return {color: "#1E90FF"};
                case '6': return {color: "#314004"};
                case '7': return {color: "#1E90FF"};
                case '8': return {color: "#314004"};
                case '9': return {color: "#314004"};
                case '10': return {color: "#314004"};
              }
            }, onEachFeature: onEachFeature
  }).addTo(map);
});

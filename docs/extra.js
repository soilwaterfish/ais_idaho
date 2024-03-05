window.onload = init;

function init() {

  const mapElement = document.getElementById("mapid");

  var map = L.map(mapElement).setView([48.5, -114.165], 13);

  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
  }).addTo(map);

  L.marker([48.5, -114.165])
    .addTo(map)
    .bindPopup("A pretty CSS popup.<br> Easily customizable.")
    .openPopup();

  L.tileLayer("");

}

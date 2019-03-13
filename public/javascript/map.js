function makeMap() {
  var commonStyles = { 'color': '#111111', 'weight': 1, 'opacity': 0.65, 'fillOpacity': 0.5 }
  var claimStyles = {
    claimed_by_you: $.extend({}, commonStyles, { 'fillColor': '#9d5fa7', 'fillOpacity': 0.8 }),
    claimed: $.extend({}, commonStyles, { 'fillColor': '#d5545a', 'fillOpacity': 0.8 }),
    quarantine: $.extend({}, commonStyles, { 'fillColor': '#2171b5', 'fillOpacity': 0.8 }),
    firstQuartile: $.extend({}, commonStyles, { 'fillColor': '#ffffcc' }),
    secondQuartile: $.extend({}, commonStyles, { 'fillColor': '#c2e699' }),
    thirdQuartile: $.extend({}, commonStyles, { 'fillColor': '#78c679' }),
    fourthQuartile: $.extend({}, commonStyles, { 'fillColor': '#238443' }),
  }

  var map = L.map('map')
  window.leafletMap = map

  var mesh_layer //Rendered map
  var last_update_bounds
  var last_update_centroid

  L.tileLayer('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map)

  $('#address_search_form').submit(function (event) {
    event.preventDefault()
    FitPcode($('#address_search').val())
  })

  var FitPcode = function (pcode) {
    $.getJSON('/pcode_get_bounds?pcode=' + pcode, function (json) {
      if (json) {
        map.fitBounds([[json.swlat, json.swlng], [json.nelat, json.nelng]])
      }
      else { alert('Postcode not found') }
    })
  }

  var FindLocation = function () {
    var lat = Cookies.get('lat')
    var lng = Cookies.get('lng')
    var pcode = Cookies.get('postcode')
    if (lat && lng) {
      var zoom = Cookies.get('zoom')
      if (zoom) {
        map.setView([lat, lng], zoom)
      }
      else {
        map.setView([lat, lng], 15)
      }
    }
    else if (pcode) {
      FitPcode(pcode)
    }
    else {
      var australia_coord = [-29.8650, 131.2094]
      map.setView(australia_coord, 5)
    }
  }

  FindLocation()

  function normalise(val, min, max) { return (val - min) / (max - min) }

  function priorityStyles(feature) {
    var propensity = normalise(feature.properties.avg_swing_propensity, 0.2, 0.45) // observed range
    var doors_knocked = feature.properties.outcomes_recorded || 0
    var total_doors = feature.properties.total_addresses_on_block || 1
    var priority = propensity * (1 - doors_knocked / total_doors)
    if (priority <= 0.25) return claimStyles.firstQuartile
    if (priority <= 0.50) return claimStyles.secondQuartile
    if (priority <= 0.75) return claimStyles.thirdQuartile
    return claimStyles.fourthQuartile
  }

  function addGeoJsonProperties(json) {
    var admin = $('#map').data('is-admin') === true

    var layer = L.geoJson(json, {
      style: function (feature) {
        switch (feature.properties.claim_status) {
        case 'claimed_by_you': return claimStyles.claimed_by_you
        case 'claimed': return claimStyles.claimed
        case 'quarantine': return claimStyles.quarantine
        default: return priorityStyles(feature)
        }
      },
      onEachFeature: function (feature, featureLayer) {
        featureLayer._leaflet_id = feature.properties.slug

        function downloadmesh(mesh_id) {
          var options = {
            slug: mesh_id,
            campaign: $('#campaign').val(),
            template: $('#template').val()
          }
          var url = LAMBDA_BASE_URL + '/map'
          $.get(url, options, function (base64str) {
            if (base64str.message == 'Internal server error') {
              return alert('This area cannot be downloaded due to a pdf rendering error, please try another area.')
            }
            //decode base64 string
            var binary = atob(base64str.base64.replace(/\s/g, ''))
            var len = binary.length
            var buffer = new ArrayBuffer(len)
            var view = new Uint8Array(buffer)
            for (var i = 0; i < len; i++) {
              view[i] = binary.charCodeAt(i)
            }

            //create the blob object
            var blob = new Blob([view], { type: 'application/pdf' })

            //create clickable URL for download
            var url = window.URL.createObjectURL(blob)
            var a = document.createElement('a')
            a.href = url
            a.download = mesh_id + '.pdf'
            a.click()
            $('#load').addClass('hidden')
          })
        }

        this.btnClaim = function () {
          var leaflet_id = this._leaflet_id
          $.post('/claim_meshblock/' + leaflet_id)
          $('.unclaim').removeClass('hidden')
          $('.download').removeClass('hidden')
          $('.claim').addClass('hidden')
          if (admin) {
            this.setStyle(claimStyles.quarantine)
          } else {
            this.setStyle(claimStyles.claimed_by_you)
          }
          $('#load').removeClass('hidden')
          downloadmesh(leaflet_id)
        }

        this.btnUnclaim = function () {
          $.post('/unclaim_meshblock/' + this._leaflet_id)
          this.setStyle(claimStyles.firstQuartile)
          $('.unclaim').addClass('hidden')
          $('.download').addClass('hidden')
          $('.admin-unclaim').addClass('hidden')
          $('.admin-download').addClass('hidden')
          $('.claim').removeClass('hidden')
        }

        this.btnDownload = function () {
          var leaflet_id = this._leaflet_id
          $('#load').removeClass('hidden')
          downloadmesh(leaflet_id)
        }

        function create_popup_btn(container, div_class, btn_text_inner, faq_text_inner) {
          var grpdiv = L.DomUtil.create('div', 'popupgrp hidden ' + div_class, container)
          var txtdiv = L.DomUtil.create('div', 'popuptxt txt' + div_class, grpdiv)
          txtdiv.innerHTML = faq_text_inner
          var btndiv = L.DomUtil.create('div', 'popupbutton btn' + div_class, grpdiv)
          var btn = L.DomUtil.create('button', '', btndiv)
          btn.setAttribute('type', 'button')
          btn.setAttribute('name', div_class)
          btn.innerHTML = btn_text_inner
          var btndom = L.DomEvent
          btndom.addListener(btn, 'click', L.DomEvent.stopPropagation)
          btndom.addListener(btn, 'click', L.DomEvent.preventDefault)
          return {
            grpdiv: grpdiv,
            btn: btn,
            btndom: btndom
          }
        }

        var container = L.DomUtil.create('div')

        L.DomUtil.create('div', 'popuptxt normal-size', container).innerHTML = 'Code: ' + feature.properties.slug
        if (feature.properties.total_addresses_on_block > 0) {
          var doors_remaining = feature.properties.total_addresses_on_block - feature.properties.outcomes_recorded
          L.DomUtil.create('div', 'popuptxt normal-size', container).innerHTML = '# of doors remaining to knock: ' + doors_remaining
          if ($('#template').val() === 'previous_results') {
            L.DomUtil.create('div', 'popuptxt normal-size', container).innerHTML = '# of voters we\'d like to speak to again: ' + feature.properties.outcomes_recorded
          }
        }
        L.DomUtil.create('hr', 'smaller-margin', container)

        var claimout = create_popup_btn(container, 'claim', 'Claim + Download', 'Click to claim area and download PDF of addresses to doorknock.<br>')
        claimout.btndom.addListener(claimout.btn, 'click', this.btnClaim, featureLayer)
        var unclaimout = create_popup_btn(container, 'unclaim', 'Unclaim', 'Click to remove your claim on this area.<br>')
        unclaimout.btndom.addListener(unclaimout.btn, 'click', this.btnUnclaim, featureLayer)
        var downloadout = create_popup_btn(container, 'download', 'Download', 'Click to download your claimed area.<br>')
        downloadout.btndom.addListener(downloadout.btn, 'click', this.btnDownload, featureLayer)
        var adminUnclaim = create_popup_btn(container, 'admin-unclaim', 'Admin Unclaim', 'Click to remove the claim on this area.<br>')
        adminUnclaim.btndom.addListener(adminUnclaim.btn, 'click', this.btnUnclaim, featureLayer)
        var adminDownload = create_popup_btn(container, 'admin-download', 'Download', 'Click to download the claimed area.<br>')
        adminDownload.btndom.addListener(adminDownload.btn, 'click', this.btnDownload, featureLayer)
        var otherstxtcontainer = L.DomUtil.create('div', 'popuptxt hidden otherstext', container)
        otherstxtcontainer.innerHTML = 'This area is claimed by someone else and is unable to be claimed.'
        var quarantinetxtcontainer = L.DomUtil.create('div', 'popuptxt hidden quarantinetext', container)
        quarantinetxtcontainer.innerHTML = 'This area is coordinated by a central event. ' +
          '<a href="' + $('#map').data('central-events-url') + '" target="_blank">Click here</a> to find it.'

        if (feature.properties.claim_status === 'claimed_by_you') {
          L.DomUtil.removeClass(unclaimout.grpdiv, 'hidden')
          L.DomUtil.removeClass(downloadout.grpdiv, 'hidden')
        }
        else if (feature.properties.claim_status === 'claimed') {
          if (admin) {
            L.DomUtil.removeClass(adminUnclaim.grpdiv, 'hidden')
            L.DomUtil.removeClass(adminDownload.grpdiv, 'hidden')
          } else {
            L.DomUtil.removeClass(otherstxtcontainer, 'hidden')
          }
        }
        else if (feature.properties.claim_status === 'quarantine') {
          if (admin) {
            L.DomUtil.removeClass(unclaimout.grpdiv, 'hidden')
            L.DomUtil.removeClass(downloadout.grpdiv, 'hidden')
          } else {
            L.DomUtil.removeClass(quarantinetxtcontainer, 'hidden')
          }
        }
        else {
          L.DomUtil.removeClass(claimout.grpdiv, 'hidden')
        }
        var popup = L.popup({}, featureLayer).setContent(container)
        featureLayer.bindPopup(popup)

      }
    })

    return layer
  }

  function getMeshblockCallback(json) {
    last_update_bounds = map.getBounds()
    last_update_centroid = map.getCenter()
    if (mesh_layer) { map.removeLayer(mesh_layer) }
    mesh_layer = addGeoJsonProperties(json)
    mesh_layer.addTo(map)
    $('#load').addClass('hidden')
  }

  var instruct = L.control()
  instruct.onAdd = function () {
    this._div = L.DomUtil.create('div', 'instruct')
    this._div.innerHTML = 'Zoom in further to load doorknockable areas.'
    this.update()
    return this._div
  }

  instruct.update = function () {
    $('.instruct').toggleClass('hidden', map.getZoom() > 14)
  }

  instruct.addTo(map)

  function updateMap(force) {
    var distance_moved, reload_dist
    var lat_lng_bnd = map.getBounds()
    var lat_lng_centroid = map.getCenter()
    Cookies.set('lat', lat_lng_centroid.lat)
    Cookies.set('lng', lat_lng_centroid.lng)
    if (last_update_centroid) {
      distance_moved = lat_lng_centroid.distanceTo(last_update_centroid)
    } else {
      distance_moved = 201
    }
    var zoom = map.getZoom()
    if (zoom > 14) {
      reload_dist = 1000 / (zoom - 14)
    } else {
      reload_dist = 0
    }
    Cookies.set('zoom', zoom)

    //Reload map if zoom not too high
    //Distance moved is not short
    //and
    //there is no last_update or the current map bounds are not within the last update's
    var moveTrigger = zoom > 14
      && (!last_update_bounds || distance_moved > reload_dist)
      && (!last_update_bounds || !last_update_bounds.contains(lat_lng_bnd))

    if (moveTrigger || force) {
      $('#load').removeClass('hidden')

      var data = {
        sey: lat_lng_bnd.getSouthWest().lat,
        sex: lat_lng_bnd.getSouthWest().lng,
        nwy: lat_lng_bnd.getNorthEast().lat,
        nwx: lat_lng_bnd.getNorthEast().lng,
      }

      $.getJSON('/meshblocks_bounds', data, getMeshblockCallback)
        .fail(function () {
          $('#load').addClass('hidden')
          alert('Error loading meshblocks. Please reload the page.')
        })
    }
    instruct.update()
  }

  map.on('moveend', function () {
    updateMap()
  })

  var legend = L.control({ position: 'bottomright' })

  legend.onAdd = function () {
    var div = L.DomUtil.create('div', 'legend')
    div.innerHTML = [
      '<i style="opacity:0.8;background:', claimStyles.fourthQuartile.fillColor, '"></i><div>Higher Priority</div>',
      '<i style="opacity:0.8;background:', claimStyles.firstQuartile.fillColor, '"></i><div>Lower Priority</div>',
      '<i style="opacity:0.8;background:', claimStyles.claimed_by_you.fillColor, '"></i><div>My area</div>',
      '<i style="opacity:0.8;background:', claimStyles.claimed.fillColor, '"></i><div>Claimed</div>',
      '<i style="opacity:0.8;background:', claimStyles.quarantine.fillColor, '"></i><div>Organised event</div>',
    ].join('')
    return div
  }

  legend.addTo(map)
  map.whenReady(updateMap)
  return updateMap
}



function windowHeight() {
  if (window.innerHeight != undefined) return window.innerHeight
  var B = document.body, D = document.documentElement
  return Math.max(B.clientHeight, D.clientHeight)
}
var headerHeight = $('.header').height()

var LAMBDA_BASE_URL = $('#map').data('lambda-base-url')
$('#map').height(windowHeight() - headerHeight)
$('#map').width('100%')
var updateLeafletMap = makeMap()


function openHelp() {
  $('#dialog').dialog({
    minWidth: 800,
    width: 800,
    height: Math.min(windowHeight() - headerHeight - 200, 800),
    beforeClose: function () { $('#dialog').addClass('hidden') }
  })
  $('#dialog').dialog({ position: { my: 'center top', at: 'center top+15%', of: window } })
  $('#dialog').removeClass('hidden')
  try { window.localStorage.setItem('helpSeen', 'true') } catch (_) { }
}
$('#faqlink').click(openHelp)

var helpSeen = 'false'
try { helpSeen = window.localStorage.getItem('helpSeen') } catch (_) { }
if (helpSeen !== 'true') $(window).load(openHelp)


$('#campaign').change(function () {
  var campaign = $('#campaign').val()
  try { window.localStorage.setItem('campaign', campaign) } catch (_) { }
})
var campaign
try { campaign = window.localStorage.getItem('campaign') } catch (_) { }
$('#campaign').val(campaign || 'warringah') //default b/c decentralised

$('#template').change(function () {
  var template = $('#template').val()
  try { window.localStorage.setItem('template', template) } catch (_) { }
  updateLeafletMap(true)
})
var template
try { template = window.localStorage.getItem('template') } catch (_) { }
$('#template').val(template || 'hidden')

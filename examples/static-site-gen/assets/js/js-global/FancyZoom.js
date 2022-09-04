// FancyZoom.js - v1.1 - http://www.fancyzoom.com
//
// Copyright (c) 2008 Cabel Sasser / Panic Inc
// All rights reserved.
// 
//     Requires: FancyZoomHTML.js
// Instructions: Include JS files in page, call setupZoom() in onLoad. That's it!
//               Any <a href> links to images will be updated to zoom inline.
//               Add rel="nozoom" to your <a href> to disable zooming for an image.
// 
// Redistribution and use of this effect in source form, with or without modification,
// are permitted provided that the following conditions are met:
// 
// * USE OF SOURCE ON COMMERCIAL (FOR-PROFIT) WEBSITE REQUIRES ONE-TIME LICENSE FEE PER DOMAIN.
//   Reasonably priced! Visit www.fancyzoom.com for licensing instructions. Thanks!
//
// * Non-commercial (personal) website use is permitted without license/payment!
//
// * Redistribution of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
//
// * Redistribution of source code and derived works cannot be sold without specific
//   written prior permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

var includeCaption = true; // Turn on the "caption" feature, and write out the caption HTML
var zoomTime       = 5;    // Milliseconds between frames of zoom animation
var zoomSteps      = 15;   // Number of zoom animation frames
var includeFade    = 1;    // Set to 1 to fade the image in / out as it zooms
var minBorder      = 90;   // Amount of padding between large, scaled down images, and the window edges
var shadowSettings = '0px 5px 25px rgba(0, 0, 0, '; // Blur, radius, color of shadow for compatible browsers

var zoomImagesURI   = '/js/images-global/zoom/'; // Location of the zoom and shadow images

// Init. Do not add anything below this line, unless it's something awesome.

var myWidth = 0, myHeight = 0, myScroll = 0; myScrollWidth = 0; myScrollHeight = 0;
var zoomOpen = false, preloadFrame = 1, preloadActive = false, preloadTime = 0, imgPreload = new Image();
var preloadAnimTimer = 0;

var zoomActive = new Array(); var zoomTimer  = new Array(); 
var zoomOrigW  = new Array(); var zoomOrigH  = new Array();
var zoomOrigX  = new Array(); var zoomOrigY  = new Array();

var zoomID         = "ZoomBox";
var theID          = "ZoomImage";
var zoomCaption    = "ZoomCaption";
var zoomCaptionDiv = "ZoomCapDiv";

if (navigator.userAgent.indexOf("MSIE") != -1) {
	var browserIsIE = true;
}

// Zoom: Setup The Page! Called in your <body>'s onLoad handler.

function setupZoom() {
	prepZooms();
	insertZoomHTML();
	zoomdiv = document.getElementById(zoomID);  
	zoomimg = document.getElementById(theID);
}

// Zoom: Inject Javascript functions into hrefs pointing to images, one by one!
// Skip any href that contains a rel="nozoom" tag.
// This is done at page load time via an onLoad() handler.

function prepZooms() {
	if (! document.getElementsByTagName) {
		return;
	}
	var links = document.getElementsByTagName("a");
	for (i = 0; i < links.length; i++) {
		if (links[i].getAttribute("href")) {
			if (links[i].getAttribute("href").search(/(.*)\.(jpg|jpeg|gif|png|bmp|tif|tiff)/gi) != -1) {
				if (links[i].getAttribute("rel") != "nozoom") {
					links[i].onclick = function (event) { return zoomClick(this, event); };
					links[i].onmouseover = function () { zoomPreload(this); };
				}
			}
		}
	}
}

// Zoom: Load an image into an image object. When done loading, function sets preloadActive to false,
// so other bits know that they can proceed with the zoom.
// Preloaded image is stored in imgPreload and swapped out in the zoom function.

function zoomPreload(from) {

	var theimage = from.getAttribute("href");

	// Only preload if we have to, i.e. the image isn't this image already

	if (imgPreload.src.indexOf(from.getAttribute("href").substr(from.getAttribute("href").lastIndexOf("/"))) == -1) {
		preloadActive = true;
		imgPreload = new Image();

		// Set a function to fire when the preload is complete, setting flags along the way.

		imgPreload.onload = function() {
			preloadActive = false;
		}

		// Load it!
		imgPreload.src = theimage;
	}
}

// Zoom: Start the preloading animation cycle.

function preloadAnimStart() {
	preloadTime = new Date();
	document.getElementById("ZoomSpin").style.left = (myWidth / 2) + 'px';
	document.getElementById("ZoomSpin").style.top = ((myHeight / 2) + myScroll) + 'px';
	document.getElementById("ZoomSpin").style.visibility = "visible";	
	preloadFrame = 1;
	document.getElementById("SpinImage").src = zoomImagesURI+'zoom-spin-'+preloadFrame+'.png';  
	preloadAnimTimer = setInterval("preloadAnim()", 100);
}

// Zoom: Display and ANIMATE the jibber-jabber widget. Once preloadActive is false, bail and zoom it up!

function preloadAnim(from) {
	if (preloadActive != false) {
		document.getElementById("SpinImage").src = zoomImagesURI+'zoom-spin-'+preloadFrame+'.png';
		preloadFrame++;
		if (preloadFrame > 12) preloadFrame = 1;
	} else {
		document.getElementById("ZoomSpin").style.visibility = "hidden";    
		clearInterval(preloadAnimTimer);
		preloadAnimTimer = 0;
		zoomIn(preloadFrom);
	}
}

// ZOOM CLICK: We got a click! Should we do the zoom? Or wait for the preload to complete?
// todo?: Double check that imgPreload src = clicked src

function zoomClick(from, evt) {

	var shift = getShift(evt);

	// Check for Command / Alt key. If pressed, pass them through -- don't zoom!
	if (! evt && window.event && (window.event.metaKey || window.event.altKey)) {
		return true;
	} else if (evt && (evt.metaKey|| evt.altKey)) {
		return true;
	}

	// Get browser dimensions
	getSize();

	// If preloading still, wait, and display the spinner.
	if (preloadActive == true) {
		// But only display the spinner if it's not already being displayed!
		if (preloadAnimTimer == 0) {
			preloadFrom = from;
			preloadAnimStart();	
		}
	} else {
		// Otherwise, we're loaded: do the zoom!
		zoomIn(from, shift);
	}
	
	return false;
	
}

// Zoom: Move an element in to endH endW, using zoomHost as a starting point.
// "from" is an object reference to the href that spawned the zoom.

function zoomIn(from, shift) {

	zoomimg.src = from.getAttribute("href");

	// Determine the zoom settings from where we came from, the element in the <a>.
	// If there's no element in the <a>, or we can't get the width, make stuff up

	if (from.childNodes[0].width) {
		startW = from.childNodes[0].width;
		startH = from.childNodes[0].height;
		startPos = findElementPos(from.childNodes[0]);
	} else {
		startW = 50;
		startH = 12;
		startPos = findElementPos(from);
	}

	hostX = startPos[0];
	hostY = startPos[1];

	// Make up for a scrolled containing div.
	// TODO: This HAS to move into findElementPos.
	
	if (document.getElementById('scroller')) {
		hostX = hostX - document.getElementById('scroller').scrollLeft;
	}

	// Determine the target zoom settings from the preloaded image object

	endW = imgPreload.width;
	endH = imgPreload.height;

	// Start! But only if we're not zooming already!

	if (zoomActive[theID] != true) {

		// Clear everything out just in case something is already open

		if (document.getElementById("ShadowBox")) {
			document.getElementById("ShadowBox").style.visibility = "hidden";
		} else if (! browserIsIE) {
		
			// Wipe timer if shadow is fading in still
			if (fadeActive["ZoomImage"]) {
				clearInterval(fadeTimer["ZoomImage"]);
				fadeActive["ZoomImage"] = false;
				fadeTimer["ZoomImage"] = false;			
			}
			
			document.getElementById("ZoomImage").style.webkitBoxShadow = shadowSettings + '0.0)';			
		}
		
		document.getElementById("ZoomClose").style.visibility = "hidden";     

		// Setup the CAPTION, if existing. Hide it first, set the text.

		if (includeCaption) {
			document.getElementById(zoomCaptionDiv).style.visibility = "hidden";
			if (from.getAttribute('title') && includeCaption) {
				// Yes, there's a caption, set it up
				document.getElementById(zoomCaption).innerHTML = from.getAttribute('title');
			} else {
				document.getElementById(zoomCaption).innerHTML = "";
			}
		}

		// Store original position in an array for future zoomOut.

		zoomOrigW[theID] = startW;
		zoomOrigH[theID] = startH;
		zoomOrigX[theID] = hostX;
		zoomOrigY[theID] = hostY;

		// Now set the starting dimensions

		zoomimg.style.width = startW + 'px';
		zoomimg.style.height = startH + 'px';
		zoomdiv.style.left = hostX + 'px';
		zoomdiv.style.top = hostY + 'px';

		// Show the zooming image container, make it invisible

		if (includeFade == 1) {
			setOpacity(0, zoomID);
		}
		zoomdiv.style.visibility = "visible";

		// If it's too big to fit in the window, shrink the width and height to fit (with ratio).

		sizeRatio = endW / endH;
		if (endW > myWidth - minBorder) {
			endW = myWidth - minBorder;
			endH = endW / sizeRatio;
		}
		if (endH > myHeight - minBorder) {
			endH = myHeight - minBorder;
			endW = endH * sizeRatio;
		}

		zoomChangeX = ((myWidth / 2) - (endW / 2) - hostX);
		zoomChangeY = (((myHeight / 2) - (endH / 2) - hostY) + myScroll);
		zoomChangeW = (endW - startW);
		zoomChangeH = (endH - startH);
		
		// Shift key?
	
		if (shift) {
			tempSteps = zoomSteps * 7;
		} else {
			tempSteps = zoomSteps;
		}

		// Setup Zoom

		zoomCurrent = 0;

		// Setup Fade with Zoom, If Requested

		if (includeFade == 1) {
			fadeCurrent = 0;
			fadeAmount = (0 - 100) / tempSteps;
		} else {
			fadeAmount = 0;
		}

		// Do It!
		
		zoomTimer[theID] = setInterval("zoomElement('"+zoomID+"', '"+theID+"', "+zoomCurrent+", "+startW+", "+zoomChangeW+", "+startH+", "+zoomChangeH+", "+hostX+", "+zoomChangeX+", "+hostY+", "+zoomChangeY+", "+tempSteps+", "+includeFade+", "+fadeAmount+", 'zoomDoneIn(zoomID)')", zoomTime);		
		zoomActive[theID] = true; 
	}
}

// Zoom it back out.

function zoomOut(from, evt) {

	// Get shift key status.
	// IE events don't seem to get passed through the function, so grab it from the window.

	if (getShift(evt)) {
		tempSteps = zoomSteps * 7;
	} else {
		tempSteps = zoomSteps;
	}	

	// Check to see if something is happening/open
  
	if (zoomActive[theID] != true) {

		// First, get rid of the shadow if necessary.

		if (document.getElementById("ShadowBox")) {
			document.getElementById("ShadowBox").style.visibility = "hidden";
		} else if (! browserIsIE) {
		
			// Wipe timer if shadow is fading in still
			if (fadeActive["ZoomImage"]) {
				clearInterval(fadeTimer["ZoomImage"]);
				fadeActive["ZoomImage"] = false;
				fadeTimer["ZoomImage"] = false;			
			}
			
			document.getElementById("ZoomImage").style.webkitBoxShadow = shadowSettings + '0.0)';			
		}

		// ..and the close box...

		document.getElementById("ZoomClose").style.visibility = "hidden";

		// ...and the caption if necessary!

		if (includeCaption && document.getElementById(zoomCaption).innerHTML != "") {
			// fadeElementSetup(zoomCaptionDiv, 100, 0, 5, 1);
			document.getElementById(zoomCaptionDiv).style.visibility = "hidden";
		}

		// Now, figure out where we came from, to get back there

		startX = parseInt(zoomdiv.style.left);
		startY = parseInt(zoomdiv.style.top);
		startW = zoomimg.width;
		startH = zoomimg.height;
		zoomChangeX = zoomOrigX[theID] - startX;
		zoomChangeY = zoomOrigY[theID] - startY;
		zoomChangeW = zoomOrigW[theID] - startW;
		zoomChangeH = zoomOrigH[theID] - startH;

		// Setup Zoom

		zoomCurrent = 0;

		// Setup Fade with Zoom, If Requested

		if (includeFade == 1) {
			fadeCurrent = 0;
			fadeAmount = (100 - 0) / tempSteps;
		} else {
			fadeAmount = 0;
		}

		// Do It!

		zoomTimer[theID] = setInterval("zoomElement('"+zoomID+"', '"+theID+"', "+zoomCurrent+", "+startW+", "+zoomChangeW+", "+startH+", "+zoomChangeH+", "+startX+", "+zoomChangeX+", "+startY+", "+zoomChangeY+", "+tempSteps+", "+includeFade+", "+fadeAmount+", 'zoomDone(zoomID, theID)')", zoomTime);	
		zoomActive[theID] = true;
	}
}

// Finished Zooming In

function zoomDoneIn(zoomdiv, theID) {

	// Note that it's open
  
	zoomOpen = true;
	zoomdiv = document.getElementById(zoomdiv);

	// Position the table shadow behind the zoomed in image, and display it

	if (document.getElementById("ShadowBox")) {

		setOpacity(0, "ShadowBox");
		shadowdiv = document.getElementById("ShadowBox");

		shadowLeft = parseInt(zoomdiv.style.left) - 13;
		shadowTop = parseInt(zoomdiv.style.top) - 8;
		shadowWidth = zoomdiv.offsetWidth + 26;
		shadowHeight = zoomdiv.offsetHeight + 26; 
	
		shadowdiv.style.width = shadowWidth + 'px';
		shadowdiv.style.height = shadowHeight + 'px';
		shadowdiv.style.left = shadowLeft + 'px';
		shadowdiv.style.top = shadowTop + 'px';

		document.getElementById("ShadowBox").style.visibility = "visible";
		fadeElementSetup("ShadowBox", 0, 100, 5);
		
	} else if (! browserIsIE) {
		// Or, do a fade of the modern shadow
		fadeElementSetup("ZoomImage", 0, .8, 5, 0, "shadow");
	}
	
	// Position and display the CAPTION, if existing
  
	if (includeCaption && document.getElementById(zoomCaption).innerHTML != "") {
		// setOpacity(0, zoomCaptionDiv);
		zoomcapd = document.getElementById(zoomCaptionDiv);
		zoomcapd.style.top = parseInt(zoomdiv.style.top) + (zoomdiv.offsetHeight + 15) + 'px';
		zoomcapd.style.left = (myWidth / 2) - (zoomcapd.offsetWidth / 2) + 'px';
		zoomcapd.style.visibility = "visible";
		// fadeElementSetup(zoomCaptionDiv, 0, 100, 5);
	}   
	
	// Display Close Box (fade it if it's not IE)

	if (!browserIsIE) setOpacity(0, "ZoomClose");
	document.getElementById("ZoomClose").style.visibility = "visible";
	if (!browserIsIE) fadeElementSetup("ZoomClose", 0, 100, 5);

	// Get keypresses
	document.onkeypress = getKey;
	
}

// Finished Zooming Out

function zoomDone(zoomdiv, theID) {

	// No longer open
  
	zoomOpen = false;

	// Clear stuff out, clean up

	zoomOrigH[theID] = "";
	zoomOrigW[theID] = "";
	document.getElementById(zoomdiv).style.visibility = "hidden";
	zoomActive[theID] == false;

	// Stop getting keypresses

	document.onkeypress = null;

}

// Actually zoom the element

function zoomElement(zoomdiv, theID, zoomCurrent, zoomStartW, zoomChangeW, zoomStartH, zoomChangeH, zoomStartX, zoomChangeX, zoomStartY, zoomChangeY, zoomSteps, includeFade, fadeAmount, execWhenDone) {

	// console.log("Zooming Step #"+zoomCurrent+ " of "+zoomSteps+" (zoom " + zoomStartW + "/" + zoomChangeW + ") (zoom " + zoomStartH + "/" + zoomChangeH + ")  (zoom " + zoomStartX + "/" + zoomChangeX + ")  (zoom " + zoomStartY + "/" + zoomChangeY + ") Fade: "+fadeAmount);
    
	// Test if we're done, or if we continue

	if (zoomCurrent == (zoomSteps + 1)) {
		zoomActive[theID] = false;
		clearInterval(zoomTimer[theID]);

		if (execWhenDone != "") {
			eval(execWhenDone);
		}
	} else {
	
		// Do the Fade!
	  
		if (includeFade == 1) {
			if (fadeAmount < 0) {
				setOpacity(Math.abs(zoomCurrent * fadeAmount), zoomdiv);
			} else {
				setOpacity(100 - (zoomCurrent * fadeAmount), zoomdiv);
			}
		}
	  
		// Calculate this step's difference, and move it!
		
		moveW = cubicInOut(zoomCurrent, zoomStartW, zoomChangeW, zoomSteps);
		moveH = cubicInOut(zoomCurrent, zoomStartH, zoomChangeH, zoomSteps);
		moveX = cubicInOut(zoomCurrent, zoomStartX, zoomChangeX, zoomSteps);
		moveY = cubicInOut(zoomCurrent, zoomStartY, zoomChangeY, zoomSteps);
	
		document.getElementById(zoomdiv).style.left = moveX + 'px';
		document.getElementById(zoomdiv).style.top = moveY + 'px';
		zoomimg.style.width = moveW + 'px';
		zoomimg.style.height = moveH + 'px';
	
		zoomCurrent++;
		
		clearInterval(zoomTimer[theID]);
		zoomTimer[theID] = setInterval("zoomElement('"+zoomdiv+"', '"+theID+"', "+zoomCurrent+", "+zoomStartW+", "+zoomChangeW+", "+zoomStartH+", "+zoomChangeH+", "+zoomStartX+", "+zoomChangeX+", "+zoomStartY+", "+zoomChangeY+", "+zoomSteps+", "+includeFade+", "+fadeAmount+", '"+execWhenDone+"')", zoomTime);
	}
}

// Zoom Utility: Get Key Press when image is open, and act accordingly

function getKey(evt) {
	if (! evt) {
		theKey = event.keyCode;
	} else {
		theKey = evt.keyCode;
	}

	if (theKey == 27) { // ESC
		zoomOut(this, evt);
	}
}

////////////////////////////
//
// FADE Functions
//

function fadeOut(elem) {
	if (elem.id) {
		fadeElementSetup(elem.id, 100, 0, 10);
	}
}

function fadeIn(elem) {
	if (elem.id) {
		fadeElementSetup(elem.id, 0, 100, 10);	
	}
}

// Fade: Initialize the fade function

var fadeActive = new Array();
var fadeQueue  = new Array();
var fadeTimer  = new Array();
var fadeClose  = new Array();
var fadeMode   = new Array();

function fadeElementSetup(theID, fdStart, fdEnd, fdSteps, fdClose, fdMode) {

	// alert("Fading: "+theID+" Steps: "+fdSteps+" Mode: "+fdMode);

	if (fadeActive[theID] == true) {
		// Already animating, queue up this command
		fadeQueue[theID] = new Array(theID, fdStart, fdEnd, fdSteps);
	} else {
		fadeSteps = fdSteps;
		fadeCurrent = 0;
		fadeAmount = (fdStart - fdEnd) / fadeSteps;
		fadeTimer[theID] = setInterval("fadeElement('"+theID+"', '"+fadeCurrent+"', '"+fadeAmount+"', '"+fadeSteps+"')", 15);
		fadeActive[theID] = true;
		fadeMode[theID] = fdMode;
		
		if (fdClose == 1) {
			fadeClose[theID] = true;
		} else {
			fadeClose[theID] = false;
		}
	}
}

// Fade: Do the fade. This function will call itself, modifying the parameters, so
// many instances can run concurrently. Can fade using opacity, or fade using a box-shadow.

function fadeElement(theID, fadeCurrent, fadeAmount, fadeSteps) {

	if (fadeCurrent == fadeSteps) {

		// We're done, so clear.

		clearInterval(fadeTimer[theID]);
		fadeActive[theID] = false;
		fadeTimer[theID] = false;

		// Should we close it once the fade is complete?

		if (fadeClose[theID] == true) {
			document.getElementById(theID).style.visibility = "hidden";
		}

		// Hang on.. did a command queue while we were working? If so, make it happen now

		if (fadeQueue[theID] && fadeQueue[theID] != false) {
			fadeElementSetup(fadeQueue[theID][0], fadeQueue[theID][1], fadeQueue[theID][2], fadeQueue[theID][3]);
			fadeQueue[theID] = false;
		}
	} else {

		fadeCurrent++;
		
		// Now actually do the fade adjustment.
		
		if (fadeMode[theID] == "shadow") {

			// Do a special fade on the webkit-box-shadow of the object
		
			if (fadeAmount < 0) {
				document.getElementById(theID).style.webkitBoxShadow = shadowSettings + (Math.abs(fadeCurrent * fadeAmount)) + ')';
			} else {
				document.getElementById(theID).style.webkitBoxShadow = shadowSettings + (100 - (fadeCurrent * fadeAmount)) + ')';
			}
			
		} else {
		
			// Set the opacity depending on if we're adding or subtracting (pos or neg)
			
			if (fadeAmount < 0) {
				setOpacity(Math.abs(fadeCurrent * fadeAmount), theID);
			} else {
				setOpacity(100 - (fadeCurrent * fadeAmount), theID);
			}
		}

		// Keep going, and send myself the updated variables
		clearInterval(fadeTimer[theID]);
		fadeTimer[theID] = setInterval("fadeElement('"+theID+"', '"+fadeCurrent+"', '"+fadeAmount+"', '"+fadeSteps+"')", 15);
	}
}

////////////////////////////
//
// UTILITY functions
//

// Utility: Set the opacity, compatible with a number of browsers. Value from 0 to 100.

function setOpacity(opacity, theID) {

	var object = document.getElementById(theID).style;

	// If it's 100, set it to 99 for Firefox.

	if (navigator.userAgent.indexOf("Firefox") != -1) {
		if (opacity == 100) { opacity = 99.9999; } // This is majorly awkward
	}

	// Multi-browser opacity setting

	object.filter = "alpha(opacity=" + opacity + ")"; // IE/Win
	object.opacity = (opacity / 100);                 // Safari 1.2, Firefox+Mozilla

}

// Utility: Math functions for animation calucations - From http://www.robertpenner.com/easing/
//
// t = time, b = begin, c = change, d = duration
// time = current frame, begin is fixed, change is basically finish - begin, duration is fixed (frames),

function linear(t, b, c, d)
{
	return c*t/d + b;
}

function sineInOut(t, b, c, d)
{
	return -c/2 * (Math.cos(Math.PI*t/d) - 1) + b;
}

function cubicIn(t, b, c, d) {
	return c*(t/=d)*t*t + b;
}

function cubicOut(t, b, c, d) {
	return c*((t=t/d-1)*t*t + 1) + b;
}

function cubicInOut(t, b, c, d)
{
	if ((t/=d/2) < 1) return c/2*t*t*t + b;
	return c/2*((t-=2)*t*t + 2) + b;
}

function bounceOut(t, b, c, d)
{
	if ((t/=d) < (1/2.75)){
		return c*(7.5625*t*t) + b;
	} else if (t < (2/2.75)){
		return c*(7.5625*(t-=(1.5/2.75))*t + .75) + b;
	} else if (t < (2.5/2.75)){
		return c*(7.5625*(t-=(2.25/2.75))*t + .9375) + b;
	} else {
		return c*(7.5625*(t-=(2.625/2.75))*t + .984375) + b;
	}
}


// Utility: Get the size of the window, and set myWidth and myHeight
// Credit to quirksmode.org

function getSize() {

	// Window Size

	if (self.innerHeight) { // Everyone but IE
		myWidth = window.innerWidth;
		myHeight = window.innerHeight;
		myScroll = window.pageYOffset;
	} else if (document.documentElement && document.documentElement.clientHeight) { // IE6 Strict
		myWidth = document.documentElement.clientWidth;
		myHeight = document.documentElement.clientHeight;
		myScroll = document.documentElement.scrollTop;
	} else if (document.body) { // Other IE, such as IE7
		myWidth = document.body.clientWidth;
		myHeight = document.body.clientHeight;
		myScroll = document.body.scrollTop;
	}

	// Page size w/offscreen areas

	if (window.innerHeight && window.scrollMaxY) {	
		myScrollWidth = document.body.scrollWidth;
		myScrollHeight = window.innerHeight + window.scrollMaxY;
	} else if (document.body.scrollHeight > document.body.offsetHeight) { // All but Explorer Mac
		myScrollWidth = document.body.scrollWidth;
		myScrollHeight = document.body.scrollHeight;
	} else { // Explorer Mac...would also work in Explorer 6 Strict, Mozilla and Safari
		myScrollWidth = document.body.offsetWidth;
		myScrollHeight = document.body.offsetHeight;
	}
}

// Utility: Get Shift Key Status
// IE events don't seem to get passed through the function, so grab it from the window.

function getShift(evt) {
	var shift = false;
	if (! evt && window.event) {
		shift = window.event.shiftKey;
	} else if (evt) {
		shift = evt.shiftKey;
		if (shift) evt.stopPropagation(); // Prevents Firefox from doing shifty things
	}
	return shift;
}

// Utility: Find the Y position of an element on a page. Return Y and X as an array

function findElementPos(elemFind)
{
	var elemX = 0;
	var elemY = 0;
	do {
		elemX += elemFind.offsetLeft;
		elemY += elemFind.offsetTop;
	} while ( elemFind = elemFind.offsetParent )

	return Array(elemX, elemY);
}
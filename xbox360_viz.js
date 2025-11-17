// ===============================
// Config
// ===============================

// Make these mutable so we can resize dynamically
let SCREEN_W = 1280;
let SCREEN_H = 720;

// Internal grid resolution for performance vs detail
const GRID_W = 300;
const GRID_H = 200;

const FFT_SIZE = 2048;
const AUDIO_SMOOTHING = 0.9;
const BASS_PULSE_SMOOTHING = 0.7;

// Color palettes - preset gradients
const PALETTES = [
  {
    name: "Psychedelic Bloom",
    hueOffset: 0.0,
    hueShiftScale: 2.0,
    satPower: 1.9,
    valBoost: 0.22
  },
  {
    name: "Xbox Classic",
    hueOffset: 0.60,
    hueShiftScale: 1.0,
    satPower: 1.0,
    valBoost: 0.0
  },
  {
    name: "Deep Ocean",
    hueOffset: 0.55,
    hueShiftScale: 0.8,
    satPower: 1.25,
    valBoost: -0.05
  },
  {
    name: "Neon Arcade",
    hueOffset: 0.80,
    hueShiftScale: 1.5,
    satPower: 1.7,
    valBoost: 0.18
  },
  {
    name: "Hyper Sunset",
    hueOffset: 0.05,
    hueShiftScale: 1.8,
    satPower: 2.0,
    valBoost: 0.25
  },
  {
    name: "Cryo Aurora",
    hueOffset: 0.45,
    hueShiftScale: 1.4,
    satPower: 1.6,
    valBoost: 0.15
  }
];

const PALETTE_FADE_TIME = 6.0;
const AUTO_PALETTE_MIN_INTERVAL = 18.0;
const AUTO_PALETTE_ENERGY_THRESHOLD = 3.0;

let currentPaletteIndex = 0;
let previousPaletteIndex = 0;
let paletteBlend = 1.0;
let autoPaletteTimer = 0.0;
let currentPalette = PALETTES[0];

// Kaleidoscope config
const KALEIDO_BASE_SEGMENTS = 8;
const KALEIDO_MIN_SEGMENTS = 6;
const KALEIDO_MAX_SEGMENTS = 24;

// ===============================
// Canvas setup
// ===============================

const mainCanvas = document.getElementById("mainCanvas");
const mainCtx = mainCanvas.getContext("2d");

const gridCanvas = document.createElement("canvas");
gridCanvas.width = GRID_W;
gridCanvas.height = GRID_H;
const gridCtx = gridCanvas.getContext("2d");

const gridImageData = gridCtx.createImageData(GRID_W, GRID_H);
const gridPixels = gridImageData.data;

// Dynamic resolution so it matches the current monitor or window
function resizeCanvas() {
  const dpr = window.devicePixelRatio || 1;

  const width = window.innerWidth || SCREEN_W;
  const height = window.innerHeight || SCREEN_H;

  mainCanvas.style.width = width + "px";
  mainCanvas.style.height = height + "px";

  mainCanvas.width = width * dpr;
  mainCanvas.height = height * dpr;

  mainCtx.setTransform(dpr, 0, 0, dpr, 0, 0);

  SCREEN_W = width;
  SCREEN_H = height;
}

resizeCanvas();
window.addEventListener("resize", resizeCanvas);

// Precompute grid
const X = new Float32Array(GRID_W * GRID_H);
const Y = new Float32Array(GRID_W * GRID_H);
const R = new Float32Array(GRID_W * GRID_H);
const Theta = new Float32Array(GRID_W * GRID_H);
const ThetaSin = new Float32Array(GRID_W * GRID_H);
const ThetaCos = new Float32Array(GRID_W * GRID_H);

(function initGrid() {
  const xMin = -1.5, xMax = 1.5;
  const yMin = -1.0, yMax = 1.0;

  for (let j = 0; j < GRID_H; j++) {
    const yVal = yMin + (yMax - yMin) * (j / (GRID_H - 1));
    for (let i = 0; i < GRID_W; i++) {
      const xVal = xMin + (xMax - xMin) * (i / (GRID_W - 1));
      const idx = j * GRID_W + i;

      X[idx] = xVal;
      Y[idx] = yVal;

      const r = Math.sqrt(xVal * xVal + yVal * yVal);
      const th = Math.atan2(yVal, xVal);

      R[idx] = r;
      Theta[idx] = th;
      ThetaSin[idx] = Math.sin(th);
      ThetaCos[idx] = Math.cos(th);
    }
  }
})();

// ===============================
// Audio globals
// ===============================

let audioCtx = null;
let analyser = null;
let timeDomainData = null;
let freqData = null;

let sampleRate = 76000;

let freqs = null;
let subBassMask = null;
let bassMask = null;
let midBassMask = null;
let midMask = null;
let trebleMask = null;

let audioVolume = 0.0;
let subBassEnergy = 0.0;
let bassEnergy = 0.0;
let midBassEnergy = 0.0;
let midEnergy = 0.0;
let trebleEnergy = 0.0;
let spectralCentroid = 0.0;

let bassPulse = 0.0;
let prevBassPulse = 0.0;
let subBassPulse = 0.0;
let prevSubBassPulse = 0.0;

let lastBlockTime = performance.now();

let lastSubBassOnsetScaled = 0.0;
let lastBassOnsetScaled = 0.0;

// Beat detection globals
let beatEnergy = 0.0;
let prevBeatEnergy = 0.0;
let beatTrigger = 0.0;
let beatPhase = 0.0;      // 0..1
let beatTempo = 2.0;      // beats per second, around 120 bpm
let beatInterval = 0.5;   // seconds between beats
let lastBeatTime = performance.now();

// ===============================
// Visual phase state
// ===============================

let phaseRadial = 0.0;
let phaseAngular = 0.0;
let phaseRipple = 0.0;
let phaseHue = 0.0;
let phasePulse = 0.0;
let phaseWarp = 0.0;

const volumeHistory = new Float32Array(7);
const bassHistory = new Float32Array(7);
let historyIndex = 0;

const dtHistory = new Float32Array(7);
dtHistory.fill(1.0 / 60.0);
let dtIndex = 0;

// ===============================
// Palette helpers
// ===============================

function lerp(a, b, t) {
  return a + (b - a) * t;
}

function getActivePalette() {
  const base = PALETTES[previousPaletteIndex];
  const target = PALETTES[currentPaletteIndex];
  const t = paletteBlend;

  return {
    hueOffset: lerp(base.hueOffset, target.hueOffset, t),
    hueShiftScale: lerp(base.hueShiftScale, target.hueShiftScale, t),
    satPower: lerp(base.satPower, target.satPower, t),
    valBoost: lerp(base.valBoost, target.valBoost, t)
  };
}

function startPaletteTransition(newIndex) {
  if (newIndex === currentPaletteIndex) return;
  previousPaletteIndex = currentPaletteIndex;
  currentPaletteIndex = newIndex;
  currentPalette = PALETTES[currentPaletteIndex];
  paletteBlend = 0.0;
}

// ===============================
// HSV to RGB
// ===============================

function hsvToRgb(h, s, v) {
  const i = Math.floor(h * 6.0);
  const f = h * 6.0 - i;
  const p = v * (1.0 - s);
  const q = v * (1.0 - f * s);
  const t = v * (1.0 - (1.0 - f) * s);

  const iMod = ((i % 6) + 6) % 6;

  let r, g, b;
  switch (iMod) {
    case 0: r = v; g = t; b = p; break;
    case 1: r = q; g = v; b = p; break;
    case 2: r = p; g = v; b = t; break;
    case 3: r = p; g = q; b = v; break;
    case 4: r = t; g = p; b = v; break;
    case 5: r = v; g = p; b = q; break;
  }
  return [r, g, b];
}

// ===============================
// FFT helpers
// ===============================

function initFftHelpers() {
  const binCount = analyser.frequencyBinCount;
  sampleRate = audioCtx.sampleRate;

  freqs = new Float32Array(binCount);
  for (let i = 0; i < binCount; i++) {
    freqs[i] = i * sampleRate / (2 * binCount);
  }

  function makeMask(low, high) {
    const mask = new Uint8Array(binCount);
    for (let i = 0; i < binCount; i++) {
      const f = freqs[i];
      if (f >= low && f < high) mask[i] = 1;
    }
    return mask;
  }

  subBassMask = makeMask(20, 60);
  bassMask = makeMask(60, 120);
  midBassMask = makeMask(120, 250);
  midMask = makeMask(250, 2000);
  trebleMask = makeMask(2000, 16000);
}

// ===============================
// Audio init
// ===============================

async function initAudio(deviceId) {
  audioCtx = new (window.AudioContext || window.webkitAudioContext)();
  analyser = audioCtx.createAnalyser();
  analyser.fftSize = FFT_SIZE;
  analyser.smoothingTimeConstant = AUDIO_SMOOTHING;

  const constraints = {
    audio: {
      deviceId: deviceId ? { exact: deviceId } : undefined,
      echoCancellation: false,
      noiseSuppression: false,
      autoGainControl: false
    }
  };

  const stream = await navigator.mediaDevices.getUserMedia(constraints);
  const source = audioCtx.createMediaStreamSource(stream);
  source.connect(analyser);

  timeDomainData = new Float32Array(analyser.fftSize);
  freqData = new Uint8Array(analyser.frequencyBinCount);

  initFftHelpers();
}

// ===============================
// Audio processing
// ===============================

function updateAudioFeatures() {
  if (!analyser) return;

  analyser.getFloatTimeDomainData(timeDomainData);
  analyser.getByteFrequencyData(freqData);

  let sum = 0.0;
  for (let i = 0; i < timeDomainData.length; i++) {
    const v = timeDomainData[i];
    sum += v * v;
  }
  const volInstant = Math.sqrt(sum / timeDomainData.length) + 1e-8;

  const binCount = freqData.length;
  const mag = freqData;

  function bandEnergy(mask) {
    let s = 0.0;
    let c = 0;
    for (let i = 0; i < binCount; i++) {
      if (mask[i]) {
        s += mag[i];
        c++;
      }
    }
    return c > 0 ? s / c : 0.0;
  }

  const subBassVal = bandEnergy(subBassMask);
  const bassVal = bandEnergy(bassMask);
  const midBassVal = bandEnergy(midBassMask);
  const midVal = bandEnergy(midMask);
  const trebVal = bandEnergy(trebleMask);

  let magSum = 0.0;
  let weightedSum = 0.0;
  for (let i = 0; i < binCount; i++) {
    const m = mag[i];
    magSum += m;
    weightedSum += freqs[i] * m;
  }
  const centroid = magSum > 0 ? (weightedSum / magSum) : 0.0;

  const sBg = AUDIO_SMOOTHING;
  audioVolume = sBg * audioVolume + (1.0 - sBg) * volInstant;
  subBassEnergy = sBg * subBassEnergy + (1.0 - sBg) * subBassVal;
  bassEnergy = sBg * bassEnergy + (1.0 - sBg) * bassVal;
  midBassEnergy = sBg * midBassEnergy + (1.0 - sBg) * midBassVal;
  midEnergy = sBg * midEnergy + (1.0 - sBg) * midVal;
  trebleEnergy = sBg * trebleEnergy + (1.0 - sBg) * trebVal;
  spectralCentroid = sBg * spectralCentroid + (1.0 - sBg) * centroid;

  const sPulse = BASS_PULSE_SMOOTHING;
  prevBassPulse = bassPulse;
  prevSubBassPulse = subBassPulse;

  bassPulse = sPulse * bassPulse + (1.0 - sPulse) * (bassVal + midBassVal * 0.5);
  subBassPulse = sPulse * subBassPulse + (1.0 - sPulse) * subBassVal;

  lastBlockTime = performance.now();

  // Onset based features
  const subBassOnset = Math.max(0.0, subBassPulse - prevSubBassPulse);
  const bassOnset = Math.max(0.0, bassPulse - prevBassPulse);

  const subBassOnsetScaled = Math.min(subBassOnset * 0.55, 9.0);
  const bassOnsetScaled = Math.min(bassOnset * 0.45, 7.0);

  lastSubBassOnsetScaled = subBassOnsetScaled;
  lastBassOnsetScaled = bassOnsetScaled;

  // Beat detection driven by low band onsets
  const rawBeatEnergy = subBassOnsetScaled * 1.2 + bassOnsetScaled;
  const now = performance.now();
  const energyRise = rawBeatEnergy - prevBeatEnergy;

  const beatEnergyThreshold = 0.9;
  const beatRiseThreshold = 0.18;
  const isBeat =
    rawBeatEnergy > beatEnergyThreshold &&
    energyRise > beatRiseThreshold;

  if (isBeat) {
    const intervalSec = (now - lastBeatTime) / 1000.0;
    if (intervalSec > 0.18 && intervalSec < 1.5) {
      beatInterval = 0.8 * beatInterval + 0.2 * intervalSec;
      beatTempo = 1.0 / beatInterval;
    }
    lastBeatTime = now;
    beatTrigger = Math.min(1.0, beatTrigger + 0.8);
  }

  beatTrigger *= 0.92;
  prevBeatEnergy = rawBeatEnergy;
  beatEnergy = rawBeatEnergy;
}

// ===============================
// Visualization
// ===============================

function renderFrame(dt) {
  // History smoothing
  volumeHistory[historyIndex] = audioVolume;
  bassHistory[historyIndex] = subBassEnergy;
  historyIndex = (historyIndex + 1) % volumeHistory.length;

  let smoothVolume = 0.0;
  let smoothSubBass = 0.0;
  for (let i = 0; i < volumeHistory.length; i++) {
    smoothVolume += volumeHistory[i];
    smoothSubBass += bassHistory[i];
  }
  smoothVolume /= volumeHistory.length;
  smoothSubBass /= bassHistory.length;

  const vol = Math.min(Math.max(smoothVolume * 130.0, 0.0), 6.0);
  const subBass = Math.min(Math.max(smoothSubBass * 0.9, 0.0), 10.0);
  const bass = Math.min(Math.max(bassEnergy * 0.7, 0.0), 9.0);
  const midBass = Math.min(Math.max(midBassEnergy * 0.45, 0.0), 8.0);
  const mids = Math.min(Math.max(midEnergy * 0.4, 0.0), 8.0);
  const treb = Math.min(Math.max(trebleEnergy * 0.35, 0.0), 8.0);

  const centNorm = Math.min(Math.max((spectralCentroid - 200.0) / (8000.0 - 200.0), 0.0), 1.0);

  const drumContinuous = Math.min(bass + midBass * 0.8, 10.0);

  // Beat tempo phase update
  const minTempo = 0.7;
  const maxTempo = 4.0;
  const tempo = Math.max(minTempo, Math.min(beatTempo || 2.0, maxTempo));
  beatPhase += tempo * dt;
  if (beatPhase > 1.0) beatPhase -= Math.floor(beatPhase);

  const beatWave = 0.5 + 0.5 * Math.sin(beatPhase * Math.PI * 2.0);
  const beatVisualPulse = beatWave * (0.35 + 0.65 * beatTrigger);

  // Palette auto transition and blending
  autoPaletteTimer += dt;
  const paletteEnergy = vol + mids + treb;
  if (autoPaletteTimer > AUTO_PALETTE_MIN_INTERVAL && paletteEnergy > AUTO_PALETTE_ENERGY_THRESHOLD) {
    const nextIndex = (currentPaletteIndex + 1) % PALETTES.length;
    startPaletteTransition(nextIndex);
    autoPaletteTimer = 0.0;
  }

  if (paletteBlend < 1.0) {
    paletteBlend = Math.min(1.0, paletteBlend + dt / PALETTE_FADE_TIME);
  }

  const activePalette = getActivePalette();
  const paletteHueOffset = activePalette.hueOffset;
  const paletteHueShiftScale = activePalette.hueShiftScale;
  const paletteSatPower = activePalette.satPower;
  const paletteValBoost = activePalette.valBoost;

  // Motion
  const zoomBase = 1.0 + subBass * 0.03 + beatVisualPulse * 0.25;
  const zoomSwing = 0.12 * Math.sin(phasePulse * 1.5);
  const zoom = zoomBase + zoomSwing;

  const radialFreq = 2.6 + subBass * 0.35 + bass * 0.22;
  const angularFreq = 1.7 + mids * 0.22;
  const rippleFreq = 4.4 + treb * 0.65;

  const radialSpeed = 0.26 + vol * 0.26;
  const angularSpeed = 0.19 + mids * 0.16;
  const rippleSpeed = 0.36 + treb * 0.24;
  const hueSpeed = 0.18 + centNorm * 0.3 + vol * 0.18;
  const warpSpeed = 0.25 + drumContinuous * 0.03;

  phasePulse += (0.55 + subBass * 0.9 + beatTrigger * 0.7) * dt;
  phaseRadial += radialSpeed * dt;
  phaseAngular += angularSpeed * dt;
  phaseRipple += rippleSpeed * dt;
  phaseHue += hueSpeed * dt;
  phaseWarp += warpSpeed * dt;

  const hueCenter = (phaseHue * 0.03) % 1.0;

  const ringRadii = [0.12, 0.18, 0.26, 0.36, 0.50, 0.70];
  const ringWidths = [0.015, 0.018, 0.022, 0.025, 0.022, 0.018];
  const ringStrengths = [1.0, 0.9, 0.8, 0.6, 0.45, 0.32];

  const pixelCount = GRID_W * GRID_H;
  const drumSegments = 18;

  // Kaleidoscope: audio reactive segment count and rotation
  let kaleidoSegments = Math.round(
    KALEIDO_BASE_SEGMENTS +
    treb * 0.6 +
    mids * 0.3
  );

  if (kaleidoSegments < KALEIDO_MIN_SEGMENTS) kaleidoSegments = KALEIDO_MIN_SEGMENTS;
  if (kaleidoSegments > KALEIDO_MAX_SEGMENTS) kaleidoSegments = KALEIDO_MAX_SEGMENTS;
  if (kaleidoSegments % 2 !== 0) kaleidoSegments += 1;

  const twoPi = Math.PI * 2.0;
  const segAngle = twoPi / kaleidoSegments;
  const kaleidoRotation = phaseAngular * 0.4 + treb * 0.03;

  // Aural vibration profile
  const percussiveEnergy = drumContinuous;
  const tonalEnergy = mids + treb;

  let percussiveFactor = Math.min(percussiveEnergy / 10.0, 1.0);
  let tonalFactor = Math.min(tonalEnergy / 12.0, 1.0);

  let auralVibrationIntensity = 0.02 + 0.07 * percussiveFactor + 0.03 * tonalFactor;
  if (auralVibrationIntensity > 0.14) auralVibrationIntensity = 0.14;

  const auralDetail = 0.6 + 0.4 * tonalFactor;

  for (let idx = 0; idx < pixelCount; idx++) {
    const rBase = R[idx];

    // Original angle
    let th = Theta[idx] + kaleidoRotation;

    // Wrap to [0, 2pi)
    th = ((th % twoPi) + twoPi) % twoPi;

    // Fold into a wedge for kaleidoscope mirroring
    let folded = th % segAngle;
    if (folded > segAngle * 0.5) {
      folded = segAngle - folded;
    }

    // Center the wedge
    const thK = folded - segAngle * 0.25;

    const tSin = Math.sin(thK);
    const tCos = Math.cos(thK);

    const radialWarp =
      0.08 * Math.sin(5.0 * rBase - phaseWarp * 1.2) +
      0.06 * Math.cos(12.0 * thK + phaseWarp * 1.8) +
      0.04 * Math.sin(8.0 * rBase + thK * 3.0 + phasePulse * 2.0);

    const Rw = rBase * (1.0 + radialWarp * (0.3 + drumContinuous * 0.03));

    const Rz = Rw * zoom;

    const waveRadial = Math.sin(radialFreq * Rz - phaseRadial);
    const angleMix = tSin * 0.5 + tCos * 0.5;
    const waveAngular = Math.sin(angularFreq * angleMix + phaseAngular);
    const waveRipple = Math.sin(rippleFreq * Rz + phaseRipple);

    let combo = waveRadial * 0.3 + waveAngular * 0.3 + waveRipple * 0.4;

    let hue = hueCenter
      + (0.22 * tSin + 0.22 * tCos) * 0.25
      + combo * 0.02
      + centNorm * 0.17
      + beatVisualPulse * 0.08;

    hue = (hue + paletteHueOffset) % 1.0;

    const motionEnergy = Math.min(Math.max(Math.abs(combo) * (0.3 + vol * 0.17), 0.25), 1.0);
    let sat = motionEnergy;

    let val = 0.6 + vol * 0.28 - Rz * 0.22 + Math.abs(waveRadial) * 0.16;
    if (val < 0.0) val = 0.0;
    if (val > 1.0) val = 1.0;

    let totalRingMask = 0.0;

    // Existing macro rings
    for (let i = 0; i < ringRadii.length; i++) {
      const baseRadius = ringRadii[i];

      const hitScale = 0.05 + 0.06 * beatVisualPulse;
      const ringRadius =
        baseRadius +
        0.025 * subBass +
        hitScale +
        0.03 * bass +
        0.012 * (i === 0 ? beatVisualPulse : 0);

      const wobbleFreq = 6 + i * 2;
      const wobbleAmp = 0.02 + 0.02 * beatTrigger;
      const ringWobble = wobbleAmp *
        Math.sin(thK * wobbleFreq + phaseAngular * (1 + i * 0.45));
      const ringRadiusField = ringRadius + ringWobble;

      const widthBase = ringWidths[i] + 0.012 * vol + 0.008 * beatTrigger;
      const ringWidth = Math.max(widthBase - 0.003 * beatVisualPulse, 0.008);
      const diff = rBase - ringRadiusField;
      const ringMask = Math.exp(-(diff * diff) / (2.0 * ringWidth * ringWidth));

      totalRingMask += ringMask * ringStrengths[i];
    }

    // New multi aural rings that respond to drums and bass intensity
    const auralRingConfigs = [
      { baseRadius: 0.18, baseWidth: 0.014, strength: 0.9 },
      { baseRadius: 0.30, baseWidth: 0.014, strength: 1.1 },
      { baseRadius: 0.42, baseWidth: 0.013, strength: 1.0 },
      { baseRadius: 0.56, baseWidth: 0.013, strength: 0.85 },
      { baseRadius: 0.70, baseWidth: 0.012, strength: 0.7 }
    ];

    for (let i = 0; i < auralRingConfigs.length; i++) {
      const cfg = auralRingConfigs[i];

      const radialDrift = auralVibrationIntensity * (0.5 + i * 0.18);

      const onsetBoost =
        0.006 * beatTrigger +
        0.004 * beatVisualPulse;

      const localRadius =
        cfg.baseRadius +
        radialDrift * (0.4 + 0.6 * Math.sin(phasePulse * (2.4 + i * 0.3) + thK * (9 + i * 3))) +
        onsetBoost;

      const localWidth = cfg.baseWidth * (0.8 + 0.7 * auralDetail);

      const diff = rBase - localRadius;
      const ringMask = Math.exp(-(diff * diff) / (2.0 * localWidth * localWidth));

      const microVibe = 0.4 + 1.6 * auralVibrationIntensity;
      const shimmer = 0.25 + 0.75 * tonalFactor;

      const modulation =
        1.0 +
        microVibe *
        (0.5 + 0.5 * Math.sin(thK * (14 + i * 4) + phasePulse * (3.0 + i * 0.4))) *
        shimmer;

      const ringContribution = ringMask * cfg.strength * modulation;

      totalRingMask += ringContribution;

      val += ringContribution * 0.55;
      sat += ringContribution * 0.22;
    }

    // Dedicated beat aural ring
    const beatRingRadiusBase = 0.22 + 0.16 * beatVisualPulse;
    const beatRingWidth = 0.016 + 0.007 * beatTrigger;
    const beatDiff = rBase - beatRingRadiusBase;
    const beatRingMask = Math.exp(-(beatDiff * beatDiff) / (2.0 * beatRingWidth * beatRingWidth)) *
      (0.4 + 0.6 * beatTrigger);

    totalRingMask += beatRingMask;
    val += beatRingMask * 0.85;
    sat += beatRingMask * 0.35;
    hue = (hue + beatRingMask * 0.08) % 1.0;

    if (totalRingMask > 4.0) totalRingMask = 4.0;

    const pulseWave = Math.sin(7.0 * Rz + phasePulse * 8.2);
    const pulseMask = Math.exp(-((pulseWave - 1) * (pulseWave - 1)) / 0.45) *
      beatTrigger * 0.35;

    const explosionStrength = beatTrigger * 0.8;
    const explosionMask = Math.exp(-rBase * 8.5) * explosionStrength;

    totalRingMask += pulseMask + explosionMask;

    const spokePattern = Math.cos(thK * drumSegments + phaseAngular * 2.4);
    const spokeMask = Math.max(spokePattern, 0.0) * drumContinuous * 0.16;
    const spokeFalloff = Math.exp(-rBase * 2.0);

    const drumSpokes = spokeMask * spokeFalloff;
    val += drumSpokes * 0.4;
    sat += drumSpokes * 0.12;
    totalRingMask += drumSpokes * 0.35;

    const tunnel = 0.25 * Math.exp(-Rw * 1.2) * (0.6 + vol * 0.25 + drumContinuous * 0.1);
    val += tunnel;
    totalRingMask += tunnel * 0.3;

    const hueShift = totalRingMask * 0.09 * paletteHueShiftScale;
    hue = (hue + hueShift) % 1.0;

    val = Math.min(val + totalRingMask * 0.8 + paletteValBoost, 1.8);

    const centerGlow = Math.exp(-rBase * 4.4) *
      (0.32 + vol * 0.2 + beatTrigger * 0.6 + drumContinuous * 0.06);
    val = Math.min(val + centerGlow, 1.9);

    let vClamped = Math.max(0.0, Math.min(val, 1.0));
    sat = Math.pow(Math.min(Math.max(sat, 0.0), 1.0), paletteSatPower);

    const [rr, gg, bb] = hsvToRgb(hue, sat, vClamped);

    const pxIndex = idx * 4;
    gridPixels[pxIndex + 0] = Math.floor(rr * 255);
    gridPixels[pxIndex + 1] = Math.floor(gg * 255);
    gridPixels[pxIndex + 2] = Math.floor(bb * 255);
    gridPixels[pxIndex + 3] = 255;
  }

  gridCtx.putImageData(gridImageData, 0, 0);
  mainCtx.drawImage(gridCanvas, 0, 0, SCREEN_W, SCREEN_H);

  drawOverlay(mainCtx);
}

// ===============================
// Overlay
// ===============================

function drawBeatFlash(ctx) {
  let strength = beatTrigger * 2.0 + beatEnergy * 0.08;
  strength = Math.max(0.0, Math.min(strength, 10.0));
  if (strength <= 0.01) return;

  const alpha = Math.max(0, Math.min(Math.floor(strength * 22.0), 220));
  if (alpha <= 0) return;

  ctx.save();
  ctx.lineWidth = 3 + strength * 1.4;
  ctx.strokeStyle = `rgba(255,255,255,${alpha / 255})`;
  ctx.strokeRect(0, 0, SCREEN_W, SCREEN_H);

  const ringCount = 3;
  for (let i = 0; i < ringCount; i++) {
    const t = (beatPhase + i / ringCount) % 1.0;
    const radius = Math.min(SCREEN_W, SCREEN_H) * (0.2 + 0.7 * t);
    const localAlpha = (alpha / 255) * (1.0 - t) * 0.6;

    ctx.beginPath();
    ctx.lineWidth = 1.5 + 3.0 * (1.0 - t) * strength * 0.1;
    ctx.strokeStyle = `rgba(255,255,255,${localAlpha})`;
    ctx.arc(SCREEN_W * 0.5, SCREEN_H * 0.5, radius, 0, Math.PI * 2);
    ctx.stroke();
  }

  ctx.restore();
}

function drawOverlay(ctx) {
  drawBeatFlash(ctx);
}

// ===============================
// Main loop
// ===============================

let lastTime = performance.now();

function frameLoop() {
  const now = performance.now();
  let rawDt = (now - lastTime) / 1000.0;
  lastTime = now;

  rawDt = Math.max(0.0, Math.min(rawDt, 0.05));

  dtHistory[dtIndex] = rawDt;
  dtIndex = (dtIndex + 1) % dtHistory.length;

  let dt = 0.0;
  for (let i = 0; i < dtHistory.length; i++) dt += dtHistory[i];
  dt /= dtHistory.length;

  if ((now - lastBlockTime) / 1000.0 > 0.7) {
    audioVolume *= 0.95;
  }

  updateAudioFeatures();
  renderFrame(dt);

  requestAnimationFrame(frameLoop);
}

// ===============================
// Device selection UI glue
// ===============================

async function populateDevices() {
  const select = document.getElementById("deviceSelect");
  const status = document.getElementById("status");
  const startBtn = document.getElementById("startBtn");

  try {
    await navigator.mediaDevices.getUserMedia({ audio: true });
    const devices = await navigator.mediaDevices.enumerateDevices();
    const inputs = devices.filter(d => d.kind === "audioinput");

    select.innerHTML = "";

    inputs.forEach((d, index) => {
      const opt = document.createElement("option");
      opt.value = d.deviceId;
      opt.textContent = d.label || `Input ${index + 1}`;
      select.appendChild(opt);
    });

    if (inputs.length === 0) {
      status.textContent = "No audio input devices found";
      startBtn.disabled = true;
    } else {
      status.textContent = "Pick BlackHole 2ch here for Spotify routing";
      startBtn.disabled = false;
    }
  } catch (err) {
    console.error(err);
    status.textContent = "Permission denied for microphone";
    startBtn.disabled = true;
  }
}

window.addEventListener("load", () => {
  const startBtn = document.getElementById("startBtn");
  const select = document.getElementById("deviceSelect");
  const status = document.getElementById("status");

  populateDevices();
  resizeCanvas();

  startBtn.addEventListener("click", async () => {
    startBtn.disabled = true;
    status.textContent = `Visualizer running - Palette: ${currentPalette.name}`;

    try {
      const deviceId = select.value || undefined;
      await initAudio(deviceId);
      lastTime = performance.now();
      frameLoop();
    } catch (err) {
      console.error(err);
      status.textContent = "Error starting audio, see console";
      startBtn.disabled = false;
    }
  });
});

// Palette hotkeys 1 to 6
window.addEventListener("keydown", (e) => {
  if (e.key >= "1" && e.key <= "6") {
    const index = parseInt(e.key, 10) - 1;
    if (index >= 0 && index < PALETTES.length) {
      startPaletteTransition(index);
      autoPaletteTimer = 0.0;
      const status = document.getElementById("status");
      if (status) {
        status.textContent = `Visualizer running - Palette: ${PALETTES[index].name}`;
      }
    }
  }
});

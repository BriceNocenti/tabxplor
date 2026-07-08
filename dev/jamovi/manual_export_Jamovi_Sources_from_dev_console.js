
// Use in Jamovi dev console (Chrome Devtools) with F10 to export all current "Sources" panel into .zip
// A. ELECTRON MAIN is straightforward  (console in unlocked)
// B. Analysis UI + results tab are more tricky (console are locked) : copy in their particular dev console ; paste manually in main dev console ; run export script in main console



// A. ELECTRON MAIN
// On main dev console, run this export script  : 
(async () => {
  const ZIP_NAME = 'jamovi_sources.zip';

  const urls = [...new Set(performance.getEntriesByType('resource').map(r => r.name))];

  const skipExt = /\.(png|jpe?g|gif|webp|bmp|ico|avif|mp4|webm|ogg|mp3|wav|flac|woff2?|ttf|otf|eot|pdf)(\?|$)/i;
  const keepExt = /\.(js|mjs|cjs|css|json|txt|xml|map|svg|html?|md|yml|yaml)(\?|$)/i;

  const candidates = urls.filter(u => {
    if (skipExt.test(u)) return false;
    if (keepExt.test(u)) return true;
    try {
      const p = new URL(u, location.href).pathname;
      return p.endsWith('/') || !p.split('/').pop().includes('.');
    } catch {
      return false;
    }
  });

  function safePath(url, contentType = '') {
    const u = new URL(url, location.href);
    let path = `${u.host}${u.pathname}`.replace(/^\/+/, '');

    if (path.endsWith('/')) {
      if (/json/i.test(contentType)) path += 'index.json';
      else if (/css/i.test(contentType)) path += 'index.css';
      else if (/javascript|ecmascript/i.test(contentType)) path += 'index.js';
      else if (/svg/i.test(contentType)) path += 'index.svg';
      else if (/html/i.test(contentType)) path += 'index.html';
      else path += 'index.txt';
    }

    if (u.search) {
      const q = u.search.slice(1).replace(/[^a-z0-9._-]+/gi, '_').slice(0, 120);
      const dot = path.lastIndexOf('.');
      path = dot > path.lastIndexOf('/') ? `${path.slice(0, dot)}__${q}${path.slice(dot)}` : `${path}__${q}`;
    }

    return path;
  }

  const files = [];

  for (const url of candidates) {
    try {
      const res = await fetch(url, { credentials: 'include' });
      if (!res.ok) continue;

      const ct = (res.headers.get('content-type') || '').toLowerCase();
      if (!(/^(text\/|application\/(javascript|json|xml|x-javascript|ld\+json))/i.test(ct) || /image\/svg\+xml/i.test(ct))) continue;

      const text = await res.text();
      files.push({ path: safePath(url, ct), text });
      console.log('added', url);
    } catch (e) {
      console.warn('skip', url, e);
    }
  }

  const report = [
    `Captured files: ${files.length}`,
    ...files.map(f => f.path)
  ].join('\n');

  files.push({ path: 'export_report.txt', text: report });

  function u16(n) { return [n & 255, n >>> 8 & 255]; }
  function u32(n) { return [n & 255, n >>> 8 & 255, n >>> 16 & 255, n >>> 24 & 255]; }
  function crc32(bytes) {
    let c = ~0;
    for (let i = 0; i < bytes.length; i++) {
      c ^= bytes[i];
      for (let k = 0; k < 8; k++) c = (c >>> 1) ^ (0xEDB88320 & -(c & 1));
    }
    return ~c >>> 0;
  }
  function enc(s) { return new TextEncoder().encode(s.replace(/\r\n/g, '\n')); }

  const parts = [];
  const central = [];
  let offset = 0;

  for (const f of files) {
    const name = enc(f.path);
    const data = enc(f.text);
    const crc = crc32(data);

    const local = [
      0x50,0x4b,0x03,0x04, 20,0,0,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0),
      ...name, ...data
    ];

    parts.push(new Uint8Array(local));

    const centralHdr = [
      0x50,0x4b,0x01,0x02, 20,0,20,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0), ...u16(0), ...u16(0), ...u16(0),
      ...u32(0), ...u32(offset),
      ...name
    ];
    central.push(new Uint8Array(centralHdr));

    offset += local.length;
  }

  const centralSize = central.reduce((n, a) => n + a.length, 0);
  const centralOffset = offset;
  const end = [
    0x50,0x4b,0x05,0x06, 0,0, 0,0,
    ...u16(files.length), ...u16(files.length),
    ...u32(centralSize), ...u32(centralOffset),
    0,0
  ];

  const blob = new Blob([...parts, ...central, new Uint8Array(end)], { type: 'application/zip' });
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = ZIP_NAME;
  document.body.appendChild(a);
  a.click();
  setTimeout(() => {
    URL.revokeObjectURL(a.href);
    a.remove();
  }, 2000);
})();






// B. Analysis UI + results tab are more tricky (console are locked) :
// 1. run this code in their particular dev console (select at the top of dev console) ; 
(async () => {
  const urls = [...new Set(performance.getEntriesByType('resource').map(r => r.name))].filter(Boolean);
  const skipSchemes = /^(devtools:|chrome:|chrome-extension:|data:|blob:|javascript:)/i;
  const candidates = urls.filter(u => !skipSchemes.test(u));

  function safePath(url, contentType = '') {
    const u = new URL(url, location.href);
    let path = `${u.host}${u.pathname}`.replace(/^\/+/, '');
    if (path.endsWith('/')) {
      if (/html/i.test(contentType)) path += 'index.html';
      else if (/json/i.test(contentType)) path += 'index.json';
      else if (/css/i.test(contentType)) path += 'index.css';
      else if (/javascript|ecmascript/i.test(contentType)) path += 'index.js';
      else if (/svg/i.test(contentType)) path += 'index.svg';
      else if (/xml/i.test(contentType)) path += 'index.xml';
      else path += 'index.bin';
    }
    if (u.search) {
      const q = u.search.slice(1).replace(/[^a-z0-9._-]+/gi, '_').slice(0, 120);
      const dot = path.lastIndexOf('.');
      path = dot > path.lastIndexOf('/') ? `${path.slice(0, dot)}__${q}${path.slice(dot)}` : `${path}__${q}`;
    }
    return path;
  }

  function guessExt(contentType) {
    if (/javascript|ecmascript/i.test(contentType)) return '.js';
    if (/css/i.test(contentType)) return '.css';
    if (/json/i.test(contentType)) return '.json';
    if (/html/i.test(contentType)) return '.html';
    if (/svg/i.test(contentType)) return '.svg';
    if (/xml/i.test(contentType)) return '.xml';
    if (/png/i.test(contentType)) return '.png';
    if (/jpe?g/i.test(contentType)) return '.jpg';
    if (/gif/i.test(contentType)) return '.gif';
    if (/webp/i.test(contentType)) return '.webp';
    if (/woff2/i.test(contentType)) return '.woff2';
    if (/woff/i.test(contentType)) return '.woff';
    if (/ttf/i.test(contentType)) return '.ttf';
    return '';
  }

  function patchPath(path, contentType) {
    const last = path.split('/').pop() || '';
    if (last.includes('.')) return path;
    const ext = guessExt(contentType);
    return ext ? `${path}${ext}` : path;
  }

  const files = [];
  const failed = [];

  for (const url of candidates) {
    try {
      const res = await fetch(url, { credentials: 'include' });
      if (!res.ok) {
        failed.push({ url, reason: `HTTP ${res.status}` });
        continue;
      }

      const contentType = (res.headers.get('content-type') || '').toLowerCase();
      const blob = await res.blob();
      const bytes = Array.from(new Uint8Array(await blob.arrayBuffer()));
      let path = safePath(url, contentType);
      path = patchPath(path, contentType);

      files.push({ path, contentType, bytes });
      console.log('added', url);
    } catch (e) {
      failed.push({ url, reason: String(e) });
      console.warn('skip', url, e);
    }
  }

  const payload = JSON.stringify({ files, failed });
  console.log('COPY_FROM_HERE_START');
  console.log(payload);
  console.log('COPY_FROM_HERE_END');
})();


// 2. Paste manually in main dev console : 
window.__jamovi_payload__ = PASTE_THE_JSON_HERE;

// 3. Run this export script in main console
(() => {
  const bucket = window.__jamovi_payload__;
  if (!bucket || !bucket.files || !bucket.files.length) {
    console.error('No payload found in window.__jamovi_payload__');
    return;
  }

  const ZIP_NAME = 'jamovi_collected_frame.zip';

  function u16(n) { return [n & 255, n >>> 8 & 255]; }
  function u32(n) { return [n & 255, n >>> 8 & 255, n >>> 16 & 255, n >>> 24 & 255]; }

  const crcTable = (() => {
    const table = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
      let c = n;
      for (let k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
      table[n] = c >>> 0;
    }
    return table;
  })();

  function crc32(bytes) {
    let c = 0xFFFFFFFF;
    for (let i = 0; i < bytes.length; i++) c = crcTable[(c ^ bytes[i]) & 0xFF] ^ (c >>> 8);
    return (c ^ 0xFFFFFFFF) >>> 0;
  }

  const enc = s => new TextEncoder().encode(s);
  const parts = [];
  const central = [];
  let offset = 0;

  for (const f of bucket.files) {
    const name = enc(f.path);
    const data = new Uint8Array(f.bytes);
    const crc = crc32(data);

    const local = new Uint8Array([
      0x50,0x4b,0x03,0x04, 20,0,0,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0),
      ...name, ...data
    ]);
    parts.push(local);

    const cen = new Uint8Array([
      0x50,0x4b,0x01,0x02, 20,0,20,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0), ...u16(0), ...u16(0), ...u16(0),
      ...u32(0), ...u32(offset), ...name
    ]);
    central.push(cen);

    offset += local.length;
  }

  const reportText = [
    `Saved files: ${bucket.files.length}`,
    `Failed files: ${bucket.failed.length}`,
    '',
    '[saved]',
    ...bucket.files.map(f => `${f.path} | ${f.contentType || 'unknown'} | ${f.bytes.length} bytes`),
    '',
    '[failed]',
    ...bucket.failed.map(f => `${f.url} | ${f.reason}`)
  ].join('\n');

  {
    const name = enc('export_report.txt');
    const data = enc(reportText);
    const crc = crc32(data);

    const local = new Uint8Array([
      0x50,0x4b,0x03,0x04, 20,0,0,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0),
      ...name, ...data
    ]);
    parts.push(local);

    const cen = new Uint8Array([
      0x50,0x4b,0x01,0x02, 20,0,20,0, 0,0, 0,0,0,0,
      ...u32(crc), ...u32(data.length), ...u32(data.length),
      ...u16(name.length), ...u16(0), ...u16(0), ...u16(0), ...u16(0),
      ...u32(0), ...u32(offset), ...name
    ]);
    central.push(cen);

    offset += local.length;
  }

  const centralSize = central.reduce((n, a) => n + a.length, 0);
  const end = new Uint8Array([
    0x50,0x4b,0x05,0x06, 0,0, 0,0,
    ...u16(bucket.files.length + 1), ...u16(bucket.files.length + 1),
    ...u32(centralSize), ...u32(offset),
    0,0
  ]);

  const zipBlob = new Blob([...parts, ...central, end], { type: 'application/zip' });
  const url = URL.createObjectURL(zipBlob);
  const a = document.createElement('a');
  a.href = url;
  a.download = ZIP_NAME;
  document.body.appendChild(a);
  a.click();
  a.remove();

  console.log('ZIP ready:', ZIP_NAME);
})();
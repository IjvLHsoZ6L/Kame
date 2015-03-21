(function () {
  'use strict';

  setTimeout(menu);

  var version = '1.1';

  var size = 4;

  var pauseBack  = [0, 1].join();
  var pauseRetry = [1, 2].join();
  var pauseMenu  = [2, 3].join();

  var obverse = document.createElement('img');
  obverse.src = 'obverse.png';
  var reverse = document.createElement('img');
  reverse.src = 'reverse.png';

  var mode, state, scale, offsetX, offsetY;

  var data = [];

  function isClear() {
    for (var i = 0; i < size; i++)
      for (var j = 0; j < size; j++)
        if (state[i][j] === 'x')
          return false;
    return true;
  }

  function exists(i, j) {
    return state[i][j] !== '.';
  }

  function pick(i, j) {
    state.count++;
    state.holding = true;
    state.i = i;
    state.j = j;
    draw();
  }

  function release() {
    state.holding = false;
    delete state.i;
    delete state.j;
    draw();
  }

  function movable(i, j) {
    if (exists(i, j))
      return false;
    if (Math.abs(i - state.i) <= 1 && Math.abs(j - state.j) <= 1)
      return false;
    if (i !== state.i && j !== state.j && Math.abs(i - state.i) !== Math.abs(j - state.j))
      return false;
    var di = i - state.i > 0 ? + 1 : i - state.i < 0 ? -1 : 0;
    var dj = j - state.j > 0 ? + 1 : j - state.j < 0 ? -1 : 0;
    var n = Math.max(Math.abs(i - state.i), Math.abs(j - state.j));
    for (var k = 1; k < n; k++)
      if (!exists(state.i + k * di, state.j + k * dj))
        return false;
    return true;
  }

  function move(i, j) {
    var di = i - state.i > 0 ? + 1 : i - state.i < 0 ? -1 : 0;
    var dj = j - state.j > 0 ? + 1 : j - state.j < 0 ? -1 : 0;
    var n  = Math.max(Math.abs(i - state.i), Math.abs(j - state.j));
    for (var k = 1; k < n; k++)
      invert(state.i + k * di, state.j + k * dj);
    state[i][j]             = state[state.i][state.j];
    state[state.i][state.j] = '.';
    state.i = i;
    state.j = j;
    draw();
  }

  function invert(i, j) {
    switch (state[i][j]) {
      case 'o':
        state[i][j] = 'x';
        return;
      case 'x':
        state[i][j] = 'o';
        return;
    }
  }

  function menu() {
    mode = 'menu';
    draw();
  }

  function begin(id) {
    mode          = 'play';
    state         = {};
    state.id      = id;
    state.count   = 0;
    state.holding = false;
    for (var i = 0; i < size; i++) {
      state[i] = [];
      for (var j = 0; j < size; j++)
        state[i][j] = data[id][j].charAt(i);
    }
    draw();
  }

  function pause() {
    mode = 'pause';
    draw();
  }

  function back() {
    mode = 'play';
    draw();
  }

  function clear() {
    mode = 'clear';
    updateRecord(state.id, state.count);
    draw();
  }

  function updateRecord(id, count) {
    var r = getRecord();
    if (!r[id] || r[id] > count) {
      r[id] = count;
      setRecord(r);
    }
  }

  function getRecord() {
    if (localStorage.getItem('FlipIt.version') === version)
      return JSON.parse(localStorage.getItem('FlipIt.record'));
    else
      return [];
  }

  function setRecord(r) {
    localStorage.setItem('FlipIt.version', version);
    localStorage.setItem('FlipIt.record', JSON.stringify(r));
  }

  function onclick(i, j) {
    var inRange = 0 <= i && i < size && 0 <= j && j < size;
    switch (mode) {
      case 'menu':
        if (inRange)
          begin(i + j * size);
        return;
      case 'play':
        if (inRange) {
          if (state.holding) {
            if (i === state.i && j === state.j)
              release();
            else if (movable(i, j)) {
              move(i, j);
              if (isClear())
                clear();
            }
            else if (exists(i, j)) {
              release();
              pick(i, j);
            }
          }
          else {
            if (exists(i, j))
              pick(i, j);
          }
        }
        else
          pause();
        return;
      case 'pause':
        switch ([i, j].join()) {
          case pauseBack:
            back();
            return;
          case pauseRetry:
            begin(state.id);
            return;
          case pauseMenu:
            menu();
            return;
          default:
            return;
        }
      case 'clear':
        menu();
        return;
    }
  }

  addEventListener('mousedown', function(event) {
    var i = Math.floor((event.clientX - offsetX) / scale);
    var j = Math.floor((event.clientY - offsetY) / scale);
    onclick(i, j);
  });

  addEventListener('touchstart', function(event) {
    event.preventDefault();
    var t = event.changedTouches[0];
    var i = Math.floor((t.clientX - offsetX) / scale);
    var j = Math.floor((t.clientY - offsetY) / scale);
    onclick(i, j);
  });

  function draw() {

    var canvas    = document.getElementById('canvas');
    canvas.width  = canvas.clientWidth;
    canvas.height = canvas.clientHeight;

    scale   = Math.floor(Math.min(canvas.width / size, canvas.height / (size + 2)));
    offsetX = Math.floor((canvas.width - size * scale) / 2);
    offsetY = Math.floor((canvas.height - size * scale) / 2);

    var context = canvas.getContext('2d');
    context.setTransform(scale, 0, 0, scale, offsetX, offsetY);
    context.font         = '10px Courier, Monospace';
    context.textAlign    = 'center';
    context.textBaseline = 'middle';
    context.lineWidth    = 0.05;
    context.lineJoin     = 'round';

    var mess = message();
    var rescale = 20;
    context.scale(1 / rescale, 1 / rescale);
    context.fillStyle = '#000';
    context.fillText(mess[0], size / 2 * rescale, (-1 / 2) * rescale, (size + 2) * rescale);
    context.fillText(mess[1], size / 2 * rescale, (size + 1 / 2) * rescale, (size + 2) * rescale);
    context.scale(rescale, rescale);

    for (var i = 0; i < size; i++) {
      for (var j = 0; j < size; j++) {
        context.setTransform(scale, 0, 0, scale, offsetX + i * scale, offsetY + j * scale);

        var bgc = backgroundColor(i, j);
        if (bgc) {
          context.fillStyle = bgc;
          context.fillRect(0.05, 0.05, 0.9, 0.9);
        }

        var bdc = borderColor(i, j);
        if (bdc) {
          context.strokeStyle = bdc;
          context.strokeRect(0.05, 0.05, 0.9, 0.9);
        }

        var txt = text(i, j);
        if (txt) {
          var rescale = 40;
          context.scale(1 / rescale, 1 / rescale);
          context.fillStyle = '#000';
          context.fillText(txt, rescale / 2, rescale / 2, rescale);
          context.scale(rescale, rescale);
        }

        var img = image(i, j);
        if (img) {
          context.drawImage(img, 0.1, 0.3, 0.8, 0.4);
        }
      }
    }
  }

  function message() {
    switch (mode) {
      case 'menu':
        return ['Flip It!', 'Choose Stage'];
      case 'play':
        return ['Count: ' + String(state.count), '[Menu]'];
      case 'pause':
        return ['Flip It!', 'ver ' + version];
      case 'clear':
        return ['Count: ' + String(state.count), 'Clear!'];
    }
  }

  function backgroundColor(i, j) {
    switch (mode) {
      case 'menu':
        var count = getRecord()[i + 4 * j];
        if (count && count === 2)
          return '#ddf';
        if (count)
          return '#dfd';
        else
          return '#ddd';
      case 'play':
      case 'clear':
        switch (state[i][j]) {
          case 'o':
            return '#ddf';
          case 'x':
            return '#fdd';
          default:
            return '#dfd';
        }
      case 'pause':
        switch([i, j].join()) {
          case pauseBack:
            return '#ddf';
          case pauseRetry:
            return '#dfd';
          case pauseMenu:
            return '#fdd';
          default:
            return '#ddd';
        }
    }
  }

  function borderColor(i, j) {
    switch (mode) {
      case 'menu':
        var count = getRecord()[i + 4 * j];
        if (count && count === 2)
          return '#bbf';
        if (count)
          return '#bfb';
        else
          return '#bbb';
      case 'play':
        if (state.holding) {
          if (i === state.i && j === state.j)
            return '#fbb';
          else if (movable(i, j))
            return '#bbf';
          else if (exists(i, j))
            return '#bfb';
          else
            return false;
        }
        else {
          if (exists(i, j))
            return '#bbf';
          else
            return false;
        }
      case 'pause':
        switch([i, j].join()) {
          case pauseBack:
            return '#bbf';
          case pauseRetry:
            return '#bfb';
          case pauseMenu:
            return '#fbb';
          default:
            return false;
        }
      default:
        return false;
    }
  }

  function text(i, j) {
    switch (mode) {
      case 'menu':
        var count = getRecord()[i + 4 * j];
        return count ? String(count) : '-';
      case 'pause':
        switch([i, j].join()) {
          case pauseBack:
            return 'Back';
          case pauseRetry:
            return 'Retry';
          case pauseMenu:
            return 'Menu';
          default:
            return false;
        }
      default:
        return false;
    }
  }

  function image(i, j) {
    switch (mode) {
      case 'play':
      case 'clear':
        switch (state[i][j]) {
          case 'o':
            return obverse;
          case 'x':
            return reverse;
          default:
            return false;
        }
      default: return false;
    }
  }

  data.push([
      '....',
      '....',
      'xxxx',
      '..xx']);
  data.push([
      '....',
      '..xx',
      '.xx.',
      'xx..']);
  data.push([
      '....',
      'x.xx',
      'x.xx',
      '.x..']);
  data.push([
      '....',
      '..xx',
      'xx..',
      'xx..']);
  data.push([
      '....',
      '.xxx',
      'xx..',
      'xx..']);
  data.push([
      '....',
      'xxxx',
      'xx..',
      '..xx']);
  data.push([
      '....',
      '.xxx',
      'xx..',
      '..xx']);
  data.push([
      '.xx.',
      'xxxx',
      '.xx.',
      '.xx.']);
  data.push([
      '..x.',
      '..x.',
      'xxxx',
      '..xx']);
  data.push([
      '..x.',
      '.xx.',
      'xxxx',
      '..x.']);
  data.push([
      'x.x.',
      '.xx.',
      'xxxx',
      '..x.']);
  data.push([
      '..x.',
      '..xx',
      'xx..',
      '.x..']);
  data.push([
      '..x.',
      '.xxx',
      'xxx.',
      '.x..']);
  data.push([
      '.xx.',
      'x.xx',
      'xx.x',
      '.xx.']);
  data.push([
      '...x',
      'xxxx',
      'x.xx',
      '.xx.']);
  data.push([
      '....',
      'xxxx',
      'xxxx',
      '.xx.']);

})();

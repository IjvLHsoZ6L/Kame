(function() {

  const size  = 4;
  const scale = 100;

  const gray  = '#cccccc';
  const red   = '#ffcccc';
  const blue  = '#ccccff';
  const green = '#ccffcc';

  const blank   = 0;
  const obverse = 1;
  const reverse = 2;

  var config;
  var count, holding, I, J;
  var state, table, image;
  var saved = [[0, 0, 0, 0], [2, 2, 2, 2], [2, 2, 2, 2], [0, 2, 2, 0]];

  (function initialize() {

    table = [];
    image = [];
    var tbody = document.getElementById('tbody');
    for (var i = 0; i < size; i++) {
      table[i] = [];
      image[i] = [];
      var tr = document.createElement('tr');
      for (var j = 0; j < size; j++) {
        table[i][j] = document.createElement('td');
        image[i][j] = document.createElement('img');
        image[i][j].style.width = (0.9 * scale) + 'px';
        table[i][j].appendChild(image[i][j]);
        table[i][j].style.width = scale + 'px';
        table[i][j].style.height = scale + 'px';
        table[i][j].style.textAlign = 'center';
        table[i][j].onclick = onclick(i, j);
        table[i][j].ontouchstart = onclick(i, j);
        tr.appendChild(table[i][j]);
      }
      tbody.appendChild(tr);
    }

    state = [];
    for (var i = 0; i < size; i++)
      state[i] = [];
    config = false;
    load();
    update();

    document.getElementById('start').onclick = function() {
      if (config) {
        for (var i = 0; i < size; i++)
          for (var j = 0; j < size; j++)
            saved[i][j] = state[i][j];
      }
      config = false;
      load();
    };

    document.getElementById('config').onclick = function() {
      config = true;
      load();
    };
  })();

  function load() {
    count = 0;
    holding = false;
    for (var i = 0; i < size; i++)
      for (var j = 0; j < size; j++)
        state[i][j] = saved[i][j];
    update();
  }

  function update() {
    document.getElementById('count').value = count;
    var isCompleted = true;
    for (var i = 0; i < size; i++) {
      for (var j = 0; j < size; j++) {
        if (state[i][j] === blank)
          image[i][j].style.visibility = 'hidden';
        else {
          image[i][j].style.visibility = 'visible';
          image[i][j].src = state[i][j] === obverse ? 'obverse.png' : 'reverse.png';
        }
        if (config)
          table[i][j].style.background = gray;
        else if (selected(i, j))
          table[i][j].style.background = red;
        else if (can_move(i, j))
          table[i][j].style.background = blue;
        else
          table[i][j].style.background = green;
        if (state[i][j] === reverse)
            isCompleted = false
      }
    }
    if (!config && !holding && isCompleted)
      setTimeout(function() { alert('Clear! [count: ' + count + ']'); }, 0);
  }

  function onclick(i, j) {
    return function() {
      if (config) {
        state[i][j] = (state[i][j] + 1) % 3;
        update();
      }
      else {
        if (selected(i, j))
          release();
        else if (can_move(i, j))
          move(i, j);
        else if (can_hold(i, j))
          hold(i, j);
      }
    };
  }

  function selected(i, j) {
    return holding && i === I && j === J;
  }

  function can_move(i, j) {
    if (!holding)
      return false;
    if (state[i][j] !== blank)
      return false;
    if (Math.abs(i - I) <= 1 && Math.abs(j - J) <= 1)
      return false;
    if (!(i === I || j === J || Math.abs(i - I) === Math.abs(j - J)))
      return false;
    var di = Math.sign(i - I);
    var dj = Math.sign(j - J);
    var ii = I;
    var jj = J;
    while (true) {
      ii += di;
      jj += dj;
      if (ii === i && jj === j)
        return true;
      if (state[ii][jj] === blank)
        return false;
    }
  }

  function can_hold(i, j) {
    return !holding && state[i][j] !== blank;
  }

  function hold(i, j) {
    holding = true;
    I = i;
    J = j;
    update();
  }

  function move(i, j) {
    var di = Math.sign(i - I);
    var dj = Math.sign(j - J);
    var ii = I;
    var jj = J;
    while (true) {
      ii += di;
      jj += dj;
      if (ii === i && jj === j)
        break;
      state[ii][jj] = state[ii][jj] === obverse ? reverse : obverse;
    }
    state[i][j] = state[I][J];
    state[I][J] = blank;
    I = i;
    J = j;
    update();
  }

  function release() {
    holding = false;
    count++;
    update();
  }
})();

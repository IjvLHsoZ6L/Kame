(function() {

  const size = 4;
  const scale = 100;

  const red = '#ffcccc';
  const green = '#ccffcc';
  const blue = '#ccccff';

  var array = [];
  var config = true;
  var count = 0;
  var holding = false;
  var cursor = { i: 0, j: 0 };

  (function() {
    var tbody = document.getElementById('tbody');
    for (var i = 0; i < size; i++) {
      array[i] = [];
      var tr = document.createElement('tr');
      tbody.appendChild(tr);
      for (var j = 0; j < size; j++) {
        array[i][j] = {};
        array[i][j].exist = false;
        array[i][j].omote = false;
        array[i][j].td = document.createElement('td');
        array[i][j].td.onclick = onclick(i, j);
        array[i][j].td.style.width = scale + 'px';
        array[i][j].td.style.height = scale + 'px';
        array[i][j].img = document.createElement('img');
        array[i][j].img.style.width = (0.9 * scale) + 'px';
        tr.appendChild(array[i][j].td);
        array[i][j].td.appendChild(array[i][j].img);
      }
    }
    update();
    document.getElementById('button').onclick = toggleConfig;
  })();

  function toggleConfig() {
    if (config) {
      config = false;
      document.getElementById('button').value = 'Config';
    }
    else {
      config = true;
      document.getElementById('button').value = 'Start';
      count = 0;
      holding = false;
      update();
    }
  }

  function update() {
    for (var i = 0; i < size; i++) {
      for (var j = 0; j < size; j++) {
        array[i][j].img.style.visibility = array[i][j].exist ? 'visible' : 'hidden';
        if (array[i][j].exist)
          array[i][j].img.src = array[i][j].omote ? 'omote.png' : 'ura.png';
        if (config)
          array[i][j].td.style.background = green;
        else if (selected(i, j))
          array[i][j].td.style.background = red;
        else if (available(i, j))
          array[i][j].td.style.background = blue;
        else
          array[i][j].td.style.background = green;
      }
    }
    document.getElementById('count').value = count;
  }

  function onclick(i, j) {
    return function() {
      if (config) {
        if (array[i][j].exist && array[i][j].omote)
          array[i][j].omote = false;
        else if (array[i][j].exist)
          array[i][j].exist = false;
        else {
          array[i][j].exist = true;
          array[i][j].omote = true;
        }
        update();
      }
      else if (selected(i, j))
        release();
      else if (available(i, j))
        move(i, j);
      else if (!holding && array[i][j].exist)
        hold(i, j);
    };
  }

  function selected(i, j) {
    return holding && i === cursor.i && j === cursor.j;
  }

  function available(i, j) {
    if (!holding)
      return false;
    if (array[i][j].exist)
      return false;
    if (Math.abs(i - cursor.i) <= 1 && Math.abs(j - cursor.j) <= 1)
      return false;
    if (!(i === cursor.i || j === cursor.j || Math.abs(i - cursor.i) === Math.abs(j - cursor.j)))
      return false;
    var di = Math.sign(i - cursor.i)
      var dj = Math.sign(j - cursor.j);
    var ii = cursor.i;
    var jj = cursor.j;
    while (true) {
      ii += di;
      jj += dj;
      if (ii === i && jj === j)
        return true;
      if (!array[ii][jj].exist)
        return false;
    }
  }

  function hold(i, j) {
    holding = true;
    cursor.i = i;
    cursor.j = j;
    update();
  }

  function move(i, j) {
    var di = Math.sign(i - cursor.i);
    var dj = Math.sign(j - cursor.j);
    var ii = cursor.i;
    var jj = cursor.j;
    while (true) {
      ii += di;
      jj += dj;
      if (ii === i && jj === j)
        break;
      array[ii][jj].omote = !array[ii][jj].omote
    }
    array[i][j].exist = true;
    array[i][j].omote = array[cursor.i][cursor.j].omote;
    array[cursor.i][cursor.j].exist = false;
    cursor.i = i;
    cursor.j = j;
    update();
  }

  function release() {
    count++;
    holding = false;
    update();
  }
})();

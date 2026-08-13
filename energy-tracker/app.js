(() => {
  "use strict";

  const state = {
    electricity: [],
    gas: [],
    rangeDays: 30,
    tableView: false,
  };

  const svgNS = "http://www.w3.org/2000/svg";

  function fmtKwh(n) {
    return n.toLocaleString(undefined, { maximumFractionDigits: 1, minimumFractionDigits: 1 });
  }

  function fmtDate(d) {
    return new Date(d + "T00:00:00").toLocaleDateString(undefined, { day: "numeric", month: "short" });
  }

  async function loadData() {
    const [elecRes, gasRes] = await Promise.all([
      fetch("data/electricity.json").catch(() => null),
      fetch("data/gas.json").catch(() => null),
    ]);

    if (!elecRes || !elecRes.ok || !gasRes || !gasRes.ok) {
      document.getElementById("data-range-note").textContent =
        "Couldn't load data/electricity.json or data/gas.json. Run fetch_consumption.py first.";
      return;
    }

    state.electricity = await elecRes.json();
    state.gas = await gasRes.json();

    const allDates = [...state.electricity.map((r) => r.date), ...state.gas.map((r) => r.date)].sort();
    const note = document.getElementById("data-range-note");
    if (allDates.length === 0) {
      note.textContent = "No consumption data yet — run fetch_consumption.py.";
    } else {
      note.textContent = `Data from ${fmtDate(allDates[0])} to ${fmtDate(allDates[allDates.length - 1])}`;
    }

    render();
  }

  function sliceRange(rows) {
    if (state.rangeDays === "all") return rows;
    return rows.slice(-state.rangeDays);
  }

  function sum(rows) {
    return rows.reduce((acc, r) => acc + r.kwh, 0);
  }

  function renderStatTile(label, valueText, unit, seriesClass, deltaText, deltaDir) {
    const tile = document.createElement("div");
    tile.className = "stat-tile";
    const l = document.createElement("div");
    l.className = "label";
    l.textContent = label;
    const v = document.createElement("div");
    v.className = "value";
    v.textContent = valueText;
    const u = document.createElement("span");
    u.className = "unit";
    u.textContent = unit;
    v.appendChild(u);
    tile.appendChild(l);
    tile.appendChild(v);
    if (deltaText) {
      const d = document.createElement("div");
      d.className = "delta" + (deltaDir ? " " + deltaDir : "");
      d.textContent = deltaText;
      tile.appendChild(d);
    }
    return tile;
  }

  function deltaBetween(current, previous) {
    if (previous === 0 || previous === undefined) return null;
    const pct = ((current - previous) / previous) * 100;
    const sign = pct >= 0 ? "+" : "";
    return { text: `${sign}${pct.toFixed(0)}% vs previous period`, dir: pct >= 0 ? "up" : "down" };
  }

  function renderStats() {
    const row = document.getElementById("stat-row");
    row.innerHTML = "";

    const buildFor = (rows) => {
      const ranged = sliceRange(rows);
      const latest = rows[rows.length - 1];
      const total = sum(ranged);
      let delta = null;
      if (state.rangeDays !== "all" && rows.length > state.rangeDays) {
        const prev = rows.slice(-state.rangeDays * 2, -state.rangeDays);
        delta = deltaBetween(total, sum(prev));
      }
      return { latest, total, delta };
    };

    const elec = buildFor(state.electricity);
    const gas = buildFor(state.gas);
    const rangeLabel = state.rangeDays === "all" ? "all time" : `last ${state.rangeDays}d`;

    if (elec.latest) {
      row.appendChild(
        renderStatTile(`Electricity — ${fmtDate(elec.latest.date)}`, fmtKwh(elec.latest.kwh), "kWh")
      );
    }
    row.appendChild(
      renderStatTile(
        `Electricity total (${rangeLabel})`,
        fmtKwh(elec.total),
        "kWh",
        null,
        elec.delta && elec.delta.text,
        elec.delta && elec.delta.dir
      )
    );
    if (gas.latest) {
      row.appendChild(renderStatTile(`Gas — ${fmtDate(gas.latest.date)}`, fmtKwh(gas.latest.kwh), "kWh"));
    }
    row.appendChild(
      renderStatTile(
        `Gas total (${rangeLabel})`,
        fmtKwh(gas.total),
        "kWh",
        null,
        gas.delta && gas.delta.text,
        gas.delta && gas.delta.dir
      )
    );
  }

  function niceMax(value) {
    if (value <= 0) return 1;
    const magnitude = Math.pow(10, Math.floor(Math.log10(value)));
    const residual = value / magnitude;
    let niceResidual;
    if (residual > 5) niceResidual = 10;
    else if (residual > 2) niceResidual = 5;
    else if (residual > 1) niceResidual = 2;
    else niceResidual = 1;
    return niceResidual * magnitude;
  }

  function buildLineChart(container, rows, colorVar) {
    container.innerHTML = "";
    if (rows.length === 0) {
      const empty = document.createElement("div");
      empty.className = "empty-state";
      empty.textContent = "No data in this range.";
      container.appendChild(empty);
      return;
    }

    const width = 900;
    const height = 240;
    const padL = 44;
    const padR = 16;
    const padT = 16;
    const padB = 28;
    const plotW = width - padL - padR;
    const plotH = height - padT - padB;

    const maxVal = niceMax(Math.max(...rows.map((r) => r.kwh)));
    const n = rows.length;

    const xAt = (i) => padL + (n === 1 ? plotW / 2 : (i / (n - 1)) * plotW);
    const yAt = (v) => padT + plotH - (v / maxVal) * plotH;

    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Daily consumption line chart");

    // gridlines + y ticks (0, mid, max)
    [0, 0.5, 1].forEach((f) => {
      const v = maxVal * f;
      const y = yAt(v);
      const line = document.createElementNS(svgNS, "line");
      line.setAttribute("x1", padL);
      line.setAttribute("x2", width - padR);
      line.setAttribute("y1", y);
      line.setAttribute("y2", y);
      line.setAttribute("stroke", "var(--gridline)");
      line.setAttribute("stroke-width", "1");
      svg.appendChild(line);

      const label = document.createElementNS(svgNS, "text");
      label.setAttribute("x", padL - 8);
      label.setAttribute("y", y + 4);
      label.setAttribute("text-anchor", "end");
      label.setAttribute("font-size", "11");
      label.setAttribute("fill", "var(--text-muted)");
      label.textContent = v.toLocaleString(undefined, { maximumFractionDigits: 0 });
      svg.appendChild(label);
    });

    // baseline
    const baseline = document.createElementNS(svgNS, "line");
    baseline.setAttribute("x1", padL);
    baseline.setAttribute("x2", width - padR);
    baseline.setAttribute("y1", yAt(0));
    baseline.setAttribute("y2", yAt(0));
    baseline.setAttribute("stroke", "var(--baseline)");
    baseline.setAttribute("stroke-width", "1");
    svg.appendChild(baseline);

    // x labels: first, middle, last
    [0, Math.floor((n - 1) / 2), n - 1].forEach((i, idx, arr) => {
      if (idx > 0 && arr[idx] === arr[idx - 1]) return;
      const label = document.createElementNS(svgNS, "text");
      label.setAttribute("x", xAt(i));
      label.setAttribute("y", height - 8);
      label.setAttribute(
        "text-anchor",
        i === 0 ? "start" : i === n - 1 ? "end" : "middle"
      );
      label.setAttribute("font-size", "11");
      label.setAttribute("fill", "var(--text-muted)");
      label.textContent = fmtDate(rows[i].date);
      svg.appendChild(label);
    });

    // area wash
    const areaPoints = rows.map((r, i) => `${xAt(i)},${yAt(r.kwh)}`).join(" L ");
    const area = document.createElementNS(svgNS, "path");
    area.setAttribute(
      "d",
      `M ${xAt(0)},${yAt(0)} L ${areaPoints} L ${xAt(n - 1)},${yAt(0)} Z`
    );
    area.setAttribute("fill", `var(${colorVar}-wash)`);
    area.setAttribute("stroke", "none");
    svg.appendChild(area);

    // line
    const line = document.createElementNS(svgNS, "path");
    line.setAttribute("d", `M ${areaPoints}`);
    line.setAttribute("fill", "none");
    line.setAttribute("stroke", `var(${colorVar})`);
    line.setAttribute("stroke-width", "2");
    line.setAttribute("stroke-linejoin", "round");
    line.setAttribute("stroke-linecap", "round");
    svg.appendChild(line);

    // end marker + direct label
    const lastX = xAt(n - 1);
    const lastY = yAt(rows[n - 1].kwh);
    const endDot = document.createElementNS(svgNS, "circle");
    endDot.setAttribute("cx", lastX);
    endDot.setAttribute("cy", lastY);
    endDot.setAttribute("r", 4);
    endDot.setAttribute("fill", `var(${colorVar})`);
    endDot.setAttribute("stroke", "var(--surface-1)");
    endDot.setAttribute("stroke-width", "2");
    svg.appendChild(endDot);

    // crosshair (hidden by default)
    const crosshair = document.createElementNS(svgNS, "line");
    crosshair.setAttribute("y1", padT);
    crosshair.setAttribute("y2", height - padB);
    crosshair.setAttribute("stroke", "var(--baseline)");
    crosshair.setAttribute("stroke-width", "1");
    crosshair.setAttribute("visibility", "hidden");
    svg.appendChild(crosshair);

    const hoverDot = document.createElementNS(svgNS, "circle");
    hoverDot.setAttribute("r", 4);
    hoverDot.setAttribute("fill", `var(${colorVar})`);
    hoverDot.setAttribute("stroke", "var(--surface-1)");
    hoverDot.setAttribute("stroke-width", "2");
    hoverDot.setAttribute("visibility", "hidden");
    svg.appendChild(hoverDot);

    // transparent hit rect for pointer tracking
    const hit = document.createElementNS(svgNS, "rect");
    hit.setAttribute("x", padL);
    hit.setAttribute("y", padT);
    hit.setAttribute("width", plotW);
    hit.setAttribute("height", plotH);
    hit.setAttribute("fill", "transparent");
    svg.appendChild(hit);

    container.appendChild(svg);

    const tooltip = document.createElement("div");
    tooltip.className = "chart-tooltip";
    tooltip.innerHTML = '<div class="tt-date"></div><div class="tt-value"></div>';
    container.appendChild(tooltip);
    const ttDate = tooltip.querySelector(".tt-date");
    const ttValue = tooltip.querySelector(".tt-value");

    function showAt(clientX) {
      const rect = svg.getBoundingClientRect();
      const relX = ((clientX - rect.left) / rect.width) * width;
      let i = Math.round(((relX - padL) / plotW) * (n - 1));
      i = Math.max(0, Math.min(n - 1, i));
      const row = rows[i];
      const x = xAt(i);
      const y = yAt(row.kwh);

      crosshair.setAttribute("x1", x);
      crosshair.setAttribute("x2", x);
      crosshair.setAttribute("visibility", "visible");
      hoverDot.setAttribute("cx", x);
      hoverDot.setAttribute("cy", y);
      hoverDot.setAttribute("visibility", "visible");

      ttDate.textContent = new Date(row.date + "T00:00:00").toLocaleDateString(undefined, {
        weekday: "short",
        day: "numeric",
        month: "short",
      });
      ttValue.textContent = `${fmtKwh(row.kwh)} kWh`;

      const px = (x / width) * rect.width;
      const py = (y / height) * rect.height;
      tooltip.style.left = `${px}px`;
      tooltip.style.top = `${py - 10}px`;
      tooltip.classList.add("visible");
    }

    function hide() {
      crosshair.setAttribute("visibility", "hidden");
      hoverDot.setAttribute("visibility", "hidden");
      tooltip.classList.remove("visible");
    }

    hit.addEventListener("pointermove", (e) => showAt(e.clientX));
    hit.addEventListener("pointerleave", hide);
    hit.addEventListener(
      "touchstart",
      (e) => {
        if (e.touches[0]) showAt(e.touches[0].clientX);
      },
      { passive: true }
    );
  }

  function buildTable(container, rows) {
    container.innerHTML = "";
    const table = document.createElement("table");
    const thead = document.createElement("thead");
    thead.innerHTML = "<tr><th>Date</th><th>kWh</th></tr>";
    const tbody = document.createElement("tbody");
    rows
      .slice()
      .reverse()
      .forEach((r) => {
        const tr = document.createElement("tr");
        const tdDate = document.createElement("td");
        tdDate.textContent = fmtDate(r.date);
        const tdVal = document.createElement("td");
        tdVal.textContent = fmtKwh(r.kwh);
        tr.appendChild(tdDate);
        tr.appendChild(tdVal);
        tbody.appendChild(tr);
      });
    table.appendChild(thead);
    table.appendChild(tbody);
    container.appendChild(table);
  }

  function render() {
    renderStats();

    const elecRows = sliceRange(state.electricity);
    const gasRows = sliceRange(state.gas);

    const elecChart = document.getElementById("electricity-chart");
    const gasChart = document.getElementById("gas-chart");
    const elecTable = document.getElementById("electricity-table");
    const gasTable = document.getElementById("gas-table");

    if (state.tableView) {
      elecChart.hidden = true;
      gasChart.hidden = true;
      elecTable.hidden = false;
      gasTable.hidden = false;
      buildTable(elecTable, elecRows);
      buildTable(gasTable, gasRows);
    } else {
      elecChart.hidden = false;
      gasChart.hidden = false;
      elecTable.hidden = true;
      gasTable.hidden = true;
      buildLineChart(elecChart, elecRows, "--series-1");
      buildLineChart(gasChart, gasRows, "--series-2");
    }
  }

  function initControls() {
    document.querySelectorAll(".filter-btn").forEach((btn) => {
      btn.addEventListener("click", () => {
        document.querySelectorAll(".filter-btn").forEach((b) => b.classList.remove("active"));
        btn.classList.add("active");
        const days = btn.dataset.days;
        state.rangeDays = days === "all" ? "all" : parseInt(days, 10);
        render();
      });
      if (parseInt(btn.dataset.days, 10) === state.rangeDays) btn.classList.add("active");
    });

    document.getElementById("table-toggle").addEventListener("click", (e) => {
      state.tableView = !state.tableView;
      e.currentTarget.setAttribute("aria-pressed", String(state.tableView));
      e.currentTarget.textContent = state.tableView ? "Chart view" : "Table view";
      render();
    });

    const themeBtn = document.getElementById("theme-toggle");
    themeBtn.addEventListener("click", () => {
      const root = document.documentElement;
      const current = root.getAttribute("data-theme");
      if (current === "dark") {
        root.setAttribute("data-theme", "light");
      } else if (current === "light") {
        root.removeAttribute("data-theme");
      } else {
        root.setAttribute("data-theme", "dark");
      }
    });

    window.addEventListener("resize", () => {
      if (!state.tableView) render();
    });
  }

  initControls();
  loadData();
})();

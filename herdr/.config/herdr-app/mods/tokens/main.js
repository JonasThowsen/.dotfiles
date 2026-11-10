// Token usage across every Herdr machine: Claude Code, Codex and opencode.
// Runs collect.py on this computer and on each connected machine, then sums
// by range. Opened as a popup (alt-u); click a range or a "by" tab to change view.

import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { exec, machines, readTextFile, appHome, theme, closeOverlay, props } from "herdr";

const RANGES = [["today", "Today"], ["7d", "7 days"], ["30d", "30 days"], ["all", "All time"]];
const GROUPS = [["tool", "Tool"], ["project", "Project"], ["model", "Model"], ["machine", "Machine"]];
const TOOL_COLOR = { "Claude Code": "ansi3", Codex: "ansi4", opencode: "ansi5" };

function fmt(n) {
  if (n >= 1e9) return (n / 1e9).toFixed(2) + "B";
  if (n >= 1e6) return (n / 1e6).toFixed(1) + "M";
  if (n >= 1e3) return (n / 1e3).toFixed(1) + "k";
  return String(n);
}

function localDay(offsetDays) {
  const d = new Date();
  d.setDate(d.getDate() - offsetDays);
  const p = (x) => String(x).padStart(2, "0");
  return `${d.getFullYear()}-${p(d.getMonth() + 1)}-${p(d.getDate())}`;
}

export default class Tokens extends View {
  init(_props, cx) {
    this.range = localStorage.getItem("range") ?? "7d";
    this.group = localStorage.getItem("group") ?? "tool";
    this.rows = []; // [machine, day, tool, model, project, in, out, cacheRead, cacheWrite]
    this.status = {}; // machine label -> "ok" | error text
    this.loading = false;
    this.updated = null;
    cx.timer.every(5 * 60 * 1000, (cx) => this.refresh(cx));
    this.refresh(cx);
  }

  refresh(cx) {
    if (this.loading) return;
    this.loading = true;
    cx.notify();
    cx.spawn(async (cx) => {
      let code;
      try {
        code = await readTextFile(`${appHome()}/mods/tokens/collect.py`);
      } catch (e) {
        this.status = { "this computer": `can't read collect.py: ${e}` };
        this.loading = false;
        cx.notify();
        return;
      }
      const targets = [{ label: "this computer", on: undefined }];
      for (const m of machines()) {
        if (m.name && m.layout !== "unavailable") targets.push({ label: m.name, on: m.name });
      }
      const results = await Promise.all(targets.map(async (t) => {
        try {
          const r = await exec("python3", ["-I", "-c", code], { on: t.on, timeout: 120000 });
          const data = JSON.parse(r.stdout);
          return { t, rows: data.rows.map((row) => [t.label, ...row]), ok: "ok" };
        } catch (e) {
          return { t, rows: [], ok: String(e?.message ?? e).slice(0, 120) };
        }
      }));
      this.rows = results.flatMap((r) => r.rows);
      this.status = Object.fromEntries(results.map((r) => [r.t.label, r.ok]));
      this.updated = new Date();
      this.loading = false;
      cx.notify();
    });
  }

  filtered() {
    const since = { today: localDay(0), "7d": localDay(6), "30d": localDay(29), all: "" }[this.range];
    return this.rows.filter((r) => r[1] >= since);
  }

  grouped(rows) {
    const idx = { machine: 0, tool: 2, model: 3, project: 4 }[this.group];
    const m = new Map();
    for (const r of rows) {
      const k = r[idx];
      const g = m.get(k) ?? { key: k, tool: r[2], inp: 0, out: 0, cr: 0, cw: 0 };
      g.inp += r[5]; g.out += r[6]; g.cr += r[7]; g.cw += r[8];
      m.set(k, g);
    }
    const list = [...m.values()].map((g) => ({ ...g, total: g.inp + g.out + g.cr + g.cw }));
    return list.sort((a, b) => b.total - a.total);
  }

  tabs(t, items, current, onPick, id) {
    return h_flex().gap(2).p(2).rounded(6).bg(t.raised).children(items.map(([k, label]) =>
      div().id(`${id}-${k}`).px(10).py(3).rounded(4).text_size(12)
        .text_color(k === current ? t.foreground : t.muted)
        .when(k === current, (el) => el.bg(t.selected))
        .hover((s) => s.bg(t.hover))
        .on_click((_e, cx) => { onPick(k); cx.notify(); })
        .child(label)));
  }

  stat(t, label, value, color) {
    return v_flex().flex_1().gap(2).p(10).rounded(6).bg(t.raised)
      .child(div().text_size(11).text_color(t.muted).child(label))
      .child(div().text_size(18).font_semibold().text_color(color ?? t.foreground).child(fmt(value)));
  }

  render(cx) {
    const t = theme();
    const rows = this.filtered();
    const sum = (i) => rows.reduce((a, r) => a + r[i], 0);
    const inp = sum(5), out = sum(6), cr = sum(7), cw = sum(8);
    const total = inp + out + cr + cw;
    const groups = this.grouped(rows);
    const max = groups[0]?.total || 1;
    const overlay = props()?.overlay;

    const header = h_flex().justify_between().items_center()
      .child(div().text_size(15).font_semibold().text_color(t.foreground).child("Token usage"))
      .child(h_flex().gap(8).items_center()
        .child(div().text_size(11).text_color(t.dim).child(
          this.loading ? "Collecting…" : this.updated ? `Updated ${this.updated.toLocaleTimeString()}` : ""))
        .child(div().id("refresh").px(8).py(2).rounded(4).text_size(12).text_color(t.muted)
          .hover((s) => s.bg(t.hover)).on_click((_e, cx) => this.refresh(cx)).child("Refresh"))
        .when(!!overlay, (el) => el.child(div().id("close").px(8).py(2).rounded(4).text_size(12)
          .text_color(t.muted).hover((s) => s.bg(t.hover))
          .on_click(() => closeOverlay(overlay)).child("✕"))));

    const list = v_flex().gap(6).children(groups.slice(0, 40).map((g, i) => {
      const color = t[TOOL_COLOR[this.group === "tool" ? g.key : g.tool]] ?? t.accent;
      return v_flex().id(`g-${i}`).gap(3)
        .child(h_flex().justify_between().text_size(12)
          .child(div().truncate().text_color(t.foreground).child(String(g.key)))
          .child(div().text_color(t.muted).child(
            `${fmt(g.total)}  ·  in ${fmt(g.inp)}  out ${fmt(g.out)}  cache ${fmt(g.cr + g.cw)}`)))
        .child(div().h(4).w_full().rounded(2).bg(t.raised)
          .child(div().h(4).rounded(2).bg(color).w(Math.max(2, (g.total / max) * ((this.size?.width ?? 700) - 32)))));
    }));

    const problems = Object.entries(this.status).filter(([, s]) => s !== "ok");

    return v_flex().id("root").on_size_change((size, cx) => { this.size = size; cx.notify(); }).size_full().bg(t.surface).p(16).gap(12).text_size(13).overflow_y_scrollbar()
      .child(header)
      .child(h_flex().justify_between().gap(8)
        .child(this.tabs(t, RANGES, this.range, (k) => { this.range = k; localStorage.setItem("range", k); }, "range"))
        .child(this.tabs(t, GROUPS, this.group, (k) => { this.group = k; localStorage.setItem("group", k); }, "group")))
      .child(h_flex().gap(8)
        .child(this.stat(t, "Total", total, t.accent))
        .child(this.stat(t, "Input", inp))
        .child(this.stat(t, "Output", out))
        .child(this.stat(t, "Cache read", cr))
        .child(this.stat(t, "Cache write", cw)))
      .child(groups.length ? list : div().text_color(t.dim).child(this.loading ? "Collecting…" : "No usage in this range."))
      .when(problems.length > 0, (el) => el.child(v_flex().gap(2).children(problems.map(([m, s]) =>
        div().text_size(11).text_color(t.blocked).child(`${m}: ${s}`)))))
      .child(div().text_size(11).text_color(t.dim).child(
        `Machines: ${Object.keys(this.status).join(", ") || "…"}  ·  Claude Code, Codex, opencode`));
  }
}

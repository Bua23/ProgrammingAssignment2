import { useEffect, useMemo, useState } from "react";
import { Job, Meta } from "./types";
import { StatTile } from "./components/StatTile";
import { CompanyBarChart } from "./components/CompanyBarChart";
import { Filters } from "./components/Filters";
import { JobCard } from "./components/JobCard";
import { EmptyState } from "./components/EmptyState";
import { formatDateTime, relativeTime } from "./format";

const DATA_BASE = `${import.meta.env.BASE_URL}data/`;
const HOUR_MS = 3_600_000;
const WINDOW_MS = 7 * 24 * HOUR_MS; // only roles from the last 7 days are shown

export default function App() {
  const [jobs, setJobs] = useState<Job[] | null>(null);
  const [unconfirmedJobs, setUnconfirmedJobs] = useState<Job[]>([]);
  const [meta, setMeta] = useState<Meta | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [search, setSearch] = useState("");
  const [company, setCompany] = useState("");
  const [role, setRole] = useState("");

  useEffect(() => {
    Promise.all([
      fetch(`${DATA_BASE}jobs.json`).then((r) => {
        if (!r.ok) throw new Error(`jobs.json: HTTP ${r.status}`);
        return r.json();
      }),
      fetch(`${DATA_BASE}meta.json`).then((r) => {
        if (!r.ok) throw new Error(`meta.json: HTTP ${r.status}`);
        return r.json();
      }),
    ])
      .then(([jobsData, metaData]) => {
        setJobs(jobsData.jobs ?? []);
        setUnconfirmedJobs(jobsData.unconfirmed_jobs ?? []);
        // Defensive: tolerate data.json files written by an older crawler
        // version that predates these fields (e.g. mid-deploy).
        setMeta({
          companies_with_matches: [],
          companies_with_unconfirmed: [],
          companies_dom_fallback: [],
          companies_failed: [],
          total_unconfirmed_found: 0,
          role_filter: null,
          ...metaData,
        });
      })
      .catch((err) => setLoadError(String(err)));
  }, []);

  const now = Date.now();
  const highlightMs = (meta?.highlight_hours ?? 48) * HOUR_MS;
  const ageOf = (iso: string | undefined | null): number =>
    iso ? now - Date.parse(iso) : Infinity;
  // Posted date when we have one, else when we first saw the role.
  const seenAtOf = (job: Job): string => job.first_seen_at ?? meta?.generated_at ?? "";
  const isNewJob = (job: Job): boolean =>
    Boolean(job.posted_at) && ageOf(job.posted_at) <= highlightMs;

  // Applied at view time as well as crawl time, so a stalled crawler can't
  // leave week-old roles on screen.
  const recentJobs = useMemo(
    () => (jobs ?? []).filter((j) => ageOf(j.posted_at) <= WINDOW_MS),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [jobs]
  );
  const recentUnconfirmed = useMemo(
    () => unconfirmedJobs.filter((j) => ageOf(j.first_seen_at ?? meta?.generated_at) <= WINDOW_MS),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [unconfirmedJobs, meta]
  );

  const filteredJobs = useMemo(() => {
    if (!jobs) return [];
    const q = search.trim().toLowerCase();
    return recentJobs.filter((job) => {
      if (company && job.company !== company) return false;
      if (role && !(job.matched_roles ?? []).includes(role)) return false;
      if (!q) return true;
      return (
        job.title.toLowerCase().includes(q) ||
        job.company.toLowerCase().includes(q) ||
        job.description_text.toLowerCase().includes(q)
      );
    });
  }, [jobs, recentJobs, search, company, role]);

  const filteredUnconfirmed = useMemo(() => {
    const q = search.trim().toLowerCase();
    return recentUnconfirmed.filter((job) => {
      if (company && job.company !== company) return false;
      if (role && !(job.matched_roles ?? []).includes(role)) return false;
      if (!q) return true;
      return job.title.toLowerCase().includes(q) || job.company.toLowerCase().includes(q);
    });
  }, [recentUnconfirmed, search, company, role]);

  const companyCounts = useMemo(() => {
    if (!jobs) return [] as [string, number][];
    const map = new Map<string, number>();
    for (const job of recentJobs) map.set(job.company, (map.get(job.company) ?? 0) + 1);
    return [...map.entries()].sort((a, b) => b[1] - a[1]);
  }, [jobs, recentJobs]);

  const allCompanies = useMemo(
    () =>
      [...new Set([...recentJobs, ...recentUnconfirmed].map((j) => j.company))].sort(),
    [recentJobs, recentUnconfirmed]
  );

  const roleLabels: Record<string, string> = meta?.role_filter ?? {};
  const roleOptions = Object.entries(roleLabels);

  return (
    <>
      <header className="hero">
        <h1>London AI leadership roles — last 7 days</h1>
        <p>
          Senior AI Manager/Lead, Customer Success, Governance, Strategy and
          Consulting roles (and adjacent ones) at ~144 tech companies, read
          from each company's own careers page. Permanent roles in London, the
          South East or remote in the UK, likely to pay £110k or more. Posted
          within the last 7 days, with the last 2 days highlighted. Refreshed
          Tuesday to Thursday at 1pm.
        </p>
      </header>

      {loadError && (
        <div className="empty-state">
          <h3>Couldn't load crawl data</h3>
          <p>{loadError} — has the crawler run at least once yet?</p>
        </div>
      )}

      {!loadError && (!jobs || !meta) && (
        <div className="empty-state">
          <h3>Loading…</h3>
        </div>
      )}

      {jobs && meta && (
        <>
          <div className="stat-grid">
            <StatTile value={recentJobs.length} label="Posted in last 7 days" />
            <StatTile value={recentJobs.filter(isNewJob).length} label={`New (last ${Math.round(highlightMs / HOUR_MS)}h)`} />
            <StatTile value={new Set(recentJobs.map((j) => j.company)).size} label="Companies hiring" />
            <StatTile value={meta.companies_checked} label="Companies tracked" />
            <StatTile value={relativeTime(meta.generated_at)} label="Last crawl" />
          </div>

          <CompanyBarChart counts={companyCounts} />

          <Filters
            search={search}
            onSearchChange={setSearch}
            company={company}
            onCompanyChange={setCompany}
            companies={allCompanies}
            role={role}
            onRoleChange={setRole}
            roles={roleOptions}
          />

          {filteredJobs.length === 0 ? (
            <EmptyState hasFilters={Boolean(search || company || role)} />
          ) : (
            <div className="job-grid">
              {filteredJobs.map((job) => (
                <JobCard job={job} roleLabels={roleLabels} isNew={isNewJob(job)} seenAt={seenAtOf(job)} key={job.id} />
              ))}
            </div>
          )}

          {filteredUnconfirmed.length > 0 && (
            <>
              <div className="section-heading">
                <h2>Also spotted on company career pages</h2>
                <p>
                  These roles were found on a company's own page, but that page
                  didn't expose a posted date, so we can't confirm when they
                  were posted. They're shown only while first seen within the
                  last 7 days, and never highlighted as new.
                </p>
              </div>
              <div className="job-grid">
                {filteredUnconfirmed.map((job) => (
                  <JobCard job={job} roleLabels={roleLabels} isNew={isNewJob(job)} seenAt={seenAtOf(job)} key={job.id} />
                ))}
              </div>
            </>
          )}

          <footer className="meta-footer">
            <div>
              <span className="status-dot" />
              Last crawl: {formatDateTime(meta.generated_at)} · window: last{" "}
              7 days · {meta.companies_checked} companies checked
              {meta.companies_failed.length > 0 && (
                <> · {meta.companies_failed.length} unreachable this run</>
              )}
              {meta.filters && (
                <>
                  {" "}· filters: {meta.filters.permanent_only ? "permanent, " : ""}
                  {meta.filters.locations.join(" / ")}
                  {meta.filters.min_salary_gbp
                    ? `, senior or £${Math.round(meta.filters.min_salary_gbp / 1000)}k+`
                    : ""}
                </>
              )}
              {meta.companies_dom_fallback.length > 0 && (
                <> · {meta.companies_dom_fallback.length} used DOM fallback (no ATS call observed)</>
              )}
            </div>
            <div>
              Sources are each company's own public careers page — no login,
              no LinkedIn scraping, no calls to the ATS made by us directly.
              Job data belongs to the respective employers; this dashboard
              only links out to their original posting.
            </div>
          </footer>
        </>
      )}
    </>
  );
}

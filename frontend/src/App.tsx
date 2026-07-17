import { useEffect, useMemo, useState } from "react";
import { Job, Meta } from "./types";
import { StatTile } from "./components/StatTile";
import { CompanyBarChart } from "./components/CompanyBarChart";
import { Filters } from "./components/Filters";
import { JobCard } from "./components/JobCard";
import { EmptyState } from "./components/EmptyState";
import { formatDateTime, relativeTime } from "./format";

const DATA_BASE = `${import.meta.env.BASE_URL}data/`;

export default function App() {
  const [jobs, setJobs] = useState<Job[] | null>(null);
  const [unconfirmedJobs, setUnconfirmedJobs] = useState<Job[]>([]);
  const [meta, setMeta] = useState<Meta | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [search, setSearch] = useState("");
  const [company, setCompany] = useState("");

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
          ...metaData,
        });
      })
      .catch((err) => setLoadError(String(err)));
  }, []);

  const filteredJobs = useMemo(() => {
    if (!jobs) return [];
    const q = search.trim().toLowerCase();
    return jobs.filter((job) => {
      if (company && job.company !== company) return false;
      if (!q) return true;
      return (
        job.title.toLowerCase().includes(q) ||
        job.company.toLowerCase().includes(q) ||
        job.description_text.toLowerCase().includes(q)
      );
    });
  }, [jobs, search, company]);

  const filteredUnconfirmed = useMemo(() => {
    const q = search.trim().toLowerCase();
    return unconfirmedJobs.filter((job) => {
      if (company && job.company !== company) return false;
      if (!q) return true;
      return job.title.toLowerCase().includes(q) || job.company.toLowerCase().includes(q);
    });
  }, [unconfirmedJobs, search, company]);

  const companyCounts = useMemo(() => {
    if (!jobs) return [] as [string, number][];
    const map = new Map<string, number>();
    for (const job of jobs) map.set(job.company, (map.get(job.company) ?? 0) + 1);
    return [...map.entries()].sort((a, b) => b[1] - a[1]);
  }, [jobs]);

  const allCompanies = useMemo(
    () =>
      [...new Set([...(jobs ?? []), ...unconfirmedJobs].map((j) => j.company))].sort(),
    [jobs, unconfirmedJobs]
  );

  return (
    <>
      <header className="hero">
        <h1>London software jobs — last 24 hours</h1>
        <p>
          A curated set of UK software product companies, crawled by rendering
          each company's own public careers page in a headless browser and
          reading the job data as their own site loads it — filtered to roles
          based in London, UK and posted within the last 24 hours. Runs
          automatically twice each weekday.
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
            <StatTile value={meta.total_jobs_found} label="Matching roles" />
            <StatTile value={meta.companies_with_matches.length} label="Companies hiring" />
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
          />

          {filteredJobs.length === 0 ? (
            <EmptyState hasFilters={Boolean(search || company)} />
          ) : (
            <div className="job-grid">
              {filteredJobs.map((job) => (
                <JobCard job={job} key={job.id} />
              ))}
            </div>
          )}

          {filteredUnconfirmed.length > 0 && (
            <>
              <div className="section-heading">
                <h2>Also spotted on company career pages</h2>
                <p>
                  These London-mentioning roles were found on a company's own
                  page, but that page didn't expose a machine-readable posted
                  date, so we can't confirm they're within the last 24 hours.
                </p>
              </div>
              <div className="job-grid">
                {filteredUnconfirmed.map((job) => (
                  <JobCard job={job} key={job.id} />
                ))}
              </div>
            </>
          )}

          <footer className="meta-footer">
            <div>
              <span className="status-dot" />
              Last crawl: {formatDateTime(meta.generated_at)} · window: last{" "}
              {meta.window_hours}h · {meta.companies_checked} companies checked
              {meta.companies_failed.length > 0 && (
                <> · {meta.companies_failed.length} unreachable this run</>
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

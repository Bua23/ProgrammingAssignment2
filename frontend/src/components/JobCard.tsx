import { Job } from "../types";
import { relativeTime } from "../format";

const KIND_LABEL: Record<string, string> = {
  london: "London",
  south_east: "South East",
  remote_uk: "Remote (UK)",
};

function sourceLabel(source: string): string {
  if (source.startsWith("dom-fallback")) return "company page";
  if (source === "jobposting-data") return "job page";
  return source;
}

export function JobCard({
  job,
  roleLabels,
  isNew,
  seenAt,
}: {
  job: Job;
  roleLabels: Record<string, string>;
  isNew: boolean; // confirmed posted date within the highlight window
  seenAt: string; // first_seen_at (used when there is no posted date)
}) {
  return (
    <article className={isNew ? "job-card job-card-new" : "job-card"}>
      <div className="company-row">
        <span className="company-name">{job.company}</span>
        <span className="posted">
          {job.posted_at
            ? `posted ${relativeTime(job.posted_at)}`
            : `first seen ${relativeTime(seenAt)}`}
        </span>
      </div>
      <h3 className="title">{job.title}</h3>
      <div className="badge-row">
        {isNew && <span className="badge badge-new">New</span>}
        {job.salary_text && <span className="badge badge-salary">{job.salary_text}</span>}
        <span className="badge">
          {job.location_kind ? KIND_LABEL[job.location_kind] : job.location}
        </span>
        <span className="badge">{sourceLabel(job.source)}</span>
        {!job.posted_at && <span className="badge badge-warning">unconfirmed</span>}
        {(job.matched_roles ?? []).map((key) => (
          <span className="badge badge-role" key={key}>
            {roleLabels[key] ?? key}
          </span>
        ))}
      </div>
      <p className="snippet">
        {job.description_snippet || "No description available from this page — open the original posting for details."}
      </p>
      <a
        className="apply-link"
        href={job.url}
        target="_blank"
        rel="noopener noreferrer"
      >
        View original posting ↗
      </a>
    </article>
  );
}

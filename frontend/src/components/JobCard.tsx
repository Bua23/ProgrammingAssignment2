import { Job } from "../types";
import { relativeTime } from "../format";

export function JobCard({
  job,
  roleLabels,
}: {
  job: Job;
  roleLabels: Record<string, string>;
}) {
  return (
    <article className="job-card">
      <div className="company-row">
        <span className="company-name">{job.company}</span>
        <span className="posted">
          {job.posted_at ? relativeTime(job.posted_at) : "date unknown"}
        </span>
      </div>
      <h3 className="title">{job.title}</h3>
      <div className="badge-row">
        <span className="badge">{job.location}</span>
        <span className="badge">{job.source}</span>
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

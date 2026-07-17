import { Job } from "../types";
import { relativeTime } from "../format";

export function JobCard({ job }: { job: Job }) {
  return (
    <article className="job-card">
      <div className="company-row">
        <span className="company-name">{job.company}</span>
        <span className="posted">{relativeTime(job.posted_at)}</span>
      </div>
      <h3 className="title">{job.title}</h3>
      <div className="badge-row">
        <span className="badge">{job.location}</span>
        <span className="badge">{job.source}</span>
      </div>
      <p className="snippet">{job.description_snippet}</p>
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

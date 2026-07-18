interface Props {
  counts: [string, number][]; // [company, count], pre-sorted descending
}

const MAX_ROWS = 15;

export function CompanyBarChart({ counts }: Props) {
  if (counts.length === 0) return null;
  const shown = counts.slice(0, MAX_ROWS);
  const max = shown[0][1];
  const hiddenCount = counts.length - shown.length;

  return (
    <div className="panel">
      <h2>Open roles by company{hiddenCount > 0 ? ` (top ${MAX_ROWS})` : ""}</h2>
      {shown.map(([company, count]) => (
        <div className="bar-row" key={company}>
          <div className="bar-label" title={company}>
            {company}
          </div>
          <div className="bar-track">
            <div
              className="bar-fill"
              style={{ width: `${Math.max(6, (count / max) * 100)}%` }}
            />
          </div>
          <div className="bar-value">{count}</div>
        </div>
      ))}
      {hiddenCount > 0 && (
        <p className="bar-more-note">+{hiddenCount} more companies with matching roles — use the filter below to browse all of them.</p>
      )}
    </div>
  );
}

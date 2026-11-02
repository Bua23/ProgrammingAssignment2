export function EmptyState({ hasFilters }: { hasFilters: boolean }) {
  return (
    <div className="empty-state">
      <h3>{hasFilters ? "No jobs match your filters" : "No matching roles in the last 7 days"}</h3>
      <p>
        {hasFilters
          ? "Try clearing the search, role or company filter."
          : "That's expected on quiet days — the tracked companies simply may not have posted a new London role since the last crawl. Check back after the next scheduled run."}
      </p>
    </div>
  );
}
